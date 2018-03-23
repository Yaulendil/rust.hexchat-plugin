/*
 * Hexchat Plugin API Bindings for Rust
 * Copyright (C) 2018 Soni L.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
//! <!-- TODO (placeholder) -->

#[doc(hidden)]
pub extern crate libc;

mod internals;

use std::panic::catch_unwind;
use std::thread;
use std::ffi::{CString, CStr};
use std::str::FromStr;

const EMPTY_CSTRING_DATA: &[u8] = b"\0";

/// A hexchat plugin
pub trait Plugin {
    fn init(&mut self, &mut PluginHandle) -> bool;
}

/// A handle for a hexchat plugin
pub struct PluginHandle {
    ph: *mut internals::Ph,
    filename: Option<CString>,
    registered: bool,
    fakehandle: *const internals::PluginGuiHandle,
}

impl PluginHandle {
    pub fn register(&mut self, name: &str, desc: &str, ver: &str) {
        unsafe {
            let ph = &mut *self.ph;
            if let Some(hexchat_plugingui_add) = ph.hexchat_plugingui_add {
                let filename = self.filename.take().expect("Attempt to re-register a plugin");
                let name = CString::new(name).unwrap();
                let desc = CString::new(desc).unwrap();
                let ver = CString::new(ver).unwrap();
                self.fakehandle = hexchat_plugingui_add(self.ph, filename.as_ptr(), name.as_ptr(), desc.as_ptr(), ver.as_ptr(), ::std::ptr::null_mut());
            }
            self.registered = true;
        }
    }
}

#[doc(hidden)]
pub unsafe fn hexchat_plugin_init<T>(plugin_handle: *mut libc::c_void,
                                     plugin_name: *mut *const libc::c_char,
                                     plugin_desc: *mut *const libc::c_char,
                                     plugin_version: *mut *const libc::c_char,
                                     arg: *const libc::c_char) -> libc::c_int
                                     where T: Plugin + Default {
    if plugin_handle.is_null() {
        // we can't really do anything here.
        eprintln!("hexchat_plugin_init called with a null plugin_handle. This is an error!");
        // TODO maybe call abort.
        return 0;
    }
    let ph = &mut *(plugin_handle as *mut internals::Ph);
    // clear the "userdata" field first thing - if the deinit function gets called (wrong hexchat
    // version, other issues), we don't wanna try to drop the hexchat_dummy or hexchat_read_fd
    // function as if it were a Box!
    ph.userdata = ::std::ptr::null_mut();
    // we set these to empty strings because rust strings aren't nul-terminated. which means we
    // need to *allocate* nul-terminated strings for the real values, and hexchat has no way of
    // freeing these.
    // BUT before we set them, read the filename off plugin_name - we'll need it!
    // (TODO figure out how to make this NOT break the plugins list)
    let filename = CStr::from_ptr(*plugin_name).to_owned();
    let empty_cstr = CStr::from_bytes_with_nul_unchecked(EMPTY_CSTRING_DATA).as_ptr();
    *plugin_name = empty_cstr;
    *plugin_desc = empty_cstr;
    *plugin_version = empty_cstr;
    // do some version checks for safety
    if let Some(hexchat_get_info) = ph.hexchat_get_info {
        let ver: *const libc::c_char = hexchat_get_info(ph, CStr::from_bytes_with_nul_unchecked(b"version\0").as_ptr());
        let cstr = CStr::from_ptr(ver);
        if let Ok(ver) = cstr.to_str() {
            let mut iter = ver.split('.');
            let a = iter.next().map(i32::from_str).and_then(Result::ok);
            let b = iter.next().map(i32::from_str).and_then(Result::ok);
            let c = iter.next().map(i32::from_str).and_then(Result::ok);
            if !match a.unwrap_or(0) {
                0 | 1 => false,
                2 => match b.unwrap_or(0) {
                    // range patterns are a bit broken
                    0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 => false,
                    9 => match c.unwrap_or(0) {
                        0 | 1 | 2 | 3 | 4 | 5 => false,
                        6 => 
                            // min acceptable version = 2.9.6
                            true,
                        _ => true,
                    },
                    _ => true,
                },
                _ => true,
            } {
                // TODO print: "error loading plugin: hexchat too old"?
                return 0;
            }
        } else {
            return 0;
        }
    }
    let r: thread::Result<Option<Box<_>>> = catch_unwind(|| {
        let mut plug = T::default();
        let mut pluginhandle = PluginHandle {
            ph: plugin_handle as *mut _,
            registered: false,
            filename: Some(filename),
            fakehandle: ::std::ptr::null(),
        };
        if plug.init(&mut pluginhandle) {
            if pluginhandle.registered {
                Some(Box::new((plug, pluginhandle.fakehandle)))
            } else {
                // TODO log: forgot to call register
                None
            }
        } else {
            None
        }
    });
    if r.is_ok() {
        if let Ok(Some(plug)) = r {
            ph.userdata = Box::into_raw(plug) as *mut libc::c_void;
            1
        } else {
            0
        }
    } else {
        // TODO try to log panic?
        0
    }
}

#[doc(hidden)]
pub unsafe fn hexchat_plugin_deinit<T>(plugin_handle: *mut libc::c_void) where T: Plugin {
    // plugin_handle should never be null, but just in case...
    if !plugin_handle.is_null() {
        let ph = &mut *(plugin_handle as *mut internals::Ph);
        if !ph.userdata.is_null() {
            // 
            let userdata = ph.userdata;
            ph.userdata = ::std::ptr::null_mut();
            // we use an explicit drop (instead of an implicit one) so this is less confusing/weird
            // to read.
            catch_unwind(|| {
                let ph = &mut *(plugin_handle as *mut internals::Ph);
                let (plug, fakehandle) = *Box::from_raw(userdata as *mut (T, *const internals::PluginGuiHandle));
                if let Some(hexchat_plugingui_remove) = ph.hexchat_plugingui_remove {
                    hexchat_plugingui_remove(ph, fakehandle);
                }
                ::std::mem::drop(plug); // suppress compiler warnings
            }).ok();
        }
    } else {
        eprintln!("hexchat_plugin_deinit called with a null plugin_handle. This is an error!");
    }
}

/// Exports a hexchat plugin.
#[macro_export]
macro_rules! hexchat_plugin {
    ($t:ty) => {
        #[no_mangle]
        pub unsafe extern "C" fn hexchat_plugin_init(plugin_handle: *mut $crate::libc::c_void,
                                              plugin_name: *mut *const $crate::libc::c_char,
                                              plugin_desc: *mut *const $crate::libc::c_char,
                                              plugin_version: *mut *const $crate::libc::c_char,
                                              arg: *const $crate::libc::c_char) -> $crate::libc::c_int {
            $crate::hexchat_plugin_init::<$t>(plugin_handle, plugin_name, plugin_desc, plugin_version, arg)
        }
        #[no_mangle]
        pub unsafe extern "C" fn hexchat_plugin_deinit(plugin_handle: *mut $crate::libc::c_void) {
            $crate::hexchat_plugin_deinit::<$t>(plugin_handle);
        }
        // unlike what the documentation states, there's no need to define hexchat_plugin_get_info.
        // so we don't. it'd be impossible to make it work well with rust anyway.
    };
}
