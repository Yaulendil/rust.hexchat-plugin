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

/*
 * Some info about how HexChat does things:
 *
 * All strings passed across the C API are UTF-8.
 * - Except `hexchat_get_info(ph, "libdirfs")`, so we need to be careful with that one.
 *
 * The pointers `name: *mut *const char, desc: *mut *const char, vers: *mut *const char` point to
 * inside the ph - that is, they're aliased. Thus, we must never convert a ph to an & or &mut
 * except as part of retrieving or setting values in it (e.g. `(*ph).hexchat_get_info` or
 * `(*ph).userdata = value`).
 *
 * `hexchat_plugin_get_info` is never used, so we omit it. It would be impractical not to.
 *
 * These cause UB:
 * `hexchat_command` may invalidate the plugin context.
 * `hexchat_find_context` and `hexchat_nickcmp` use the plugin context without checking it.
 * `hexchat_get_prefs` uses the plugin context if name == "state_cursor" or "id" (or anything with
 * the same hash).
 * `hexchat_list_get` uses the plugin context if name == "notify" (or anything with the same hash).
 * `hexchat_list_str`, `hexchat_list_int`, 
 * `hexchat_emit_print`, `hexchat_emit_print_attrs` use the plugin context.
 * `hexchat_send_modes` uses the plugin context.
 * We need to wrap them (or, alternatively, hexchat_command). However, there's no (safe) way to get
 * a valid context afterwards.
 * - Actually that's a lie. Hexchat does a trick to give you a context as part of the channel list.
 *     We can use that to our advantage. I'm not sure if it's better to wrap hexchat_command or the
 *     other functions, tho.
 *     (Do we want to walk a linked list every time we use hexchat_command? I'd think
 *     hexchat_command is the most used API function... Plus, emit_print could indirectly
 *     invalidate the context as well.)
 *
 * `hexchat_send_modes` should only be used with channels; however, it doesn't cause UB - it just
 * doesn't work.
 *
 * `hexchat_pluginpref_get_int`, `hexchat_pluginpref_get_str`, `hexchat_pluginpref_set_int`,
 * `hexchat_pluginpref_set_str` cannot be used while `name`, `desc`, `vers` are null.
 *
 * `hexchat_plugin_init` receives an arg string. it may be null. this isn't documented anywhere.
 */

/*
 * Some info about how we do things:
 *
 * DO NOT CALL printf/commandf/etc FAMILY OF FUNCTIONS. You can't avoid allocations, so just
 * allocate some CStrings on the Rust side. It has the exact same effect, since those functions
 * allocate internally anyway.
 */

/*
 * Big list o' TODO:
 * -[ ] Finish API support. [PRI-HIGH]
 *     -[x] word
 *     -[x] word_eol
 *     -[ ] HEXCHAT_PRI_{HIGHEST, HIGH, NORM, LOW, LOWEST}
 *     -[x] HEXCHAT_EAT_{NONE, HEXCHAT, PLUGIN, ALL}
 *     -[ ] HEXCHAT_FD_{READ, WRITE, EXCEPTION, NOTSOCKET}
 *     -[x] hexchat_command (for commandf, use command(&format!("...")), it is equivalent.)
 *     -[x] hexchat_print (for printf, use print(&format!("...")), it is equivalent.)
 *     -[ ] hexchat_emit_print
 *     -[ ] hexchat_emit_print_attrs
 *     -[ ] hexchat_send_modes
 *     -[ ] hexchat_nickcmp
 *     -[ ] hexchat_strip
 *     -[x] ~~hexchat_free~~ not available - use Drop impls.
 *     -[ ] hexchat_event_attrs_create
 *     -[x] ~~hexchat_event_attrs_free~~ not available - use Drop impls.
 *     -[x] hexchat_get_info
 *     -[ ] hexchat_get_prefs
 *     -[ ] hexchat_list_get, hexchat_list_fields, hexchat_list_next, hexchat_list_str,
 *         hexchat_list_int, hexchat_list_time, hexchat_list_free
 *     -[x] hexchat_hook_command
 *     -[ ] hexchat_hook_fd
 *     -[x] hexchat_hook_print
 *     -[ ] hexchat_hook_print_attrs
 *     -[x] hexchat_hook_server
 *     -[ ] hexchat_hook_server_attrs
 *     -[x] hexchat_hook_timer
 *     -[x] ~~hexchat_unhook~~ not available - use Drop impls
 *     -[x] hexchat_find_context
 *     -[x] hexchat_get_context
 *     -[x] hexchat_set_context
 *     -[ ] hexchat_pluginpref_set_str
 *     -[ ] hexchat_pluginpref_get_str
 *     -[ ] hexchat_pluginpref_set_int
 *     -[ ] hexchat_pluginpref_get_int
 *     -[ ] hexchat_pluginpref_delete
 *     -[ ] hexchat_pluginpref_list
 *     -[ ] hexchat_plugingui_add
 *     -[x] ~~hexchat_plugingui_remove~~ not available - use Drop impls.
 * -[ ] Wrap contexts within something we keep track of. Mark them invalid when contexts are
 *     closed. [PRI-MAYBE]
 * -[x] Anchor closures on the stack using Rc<T>. Many (most?) hooks are reentrant. As far as I
 *     know, all of them need this.
 *     -[x] Additionally, use a Cell<bool> for timers.
 * -[ ] ???
 */

#[doc(hidden)]
pub extern crate libc;

mod internals;

use std::panic::catch_unwind;
use std::thread;
use std::ffi::{CString, CStr};
use std::str::FromStr;
use std::mem;
use std::ptr;
use std::marker::PhantomData;
use std::ops;
use std::rc::Rc;
use std::cell::Cell;
use std::borrow::Cow;

// ****** //
// PUBLIC //
// ****** //

/// A hexchat plugin
pub trait Plugin {
    /// Called to initialize the plugin.
    fn init(&self, ph: &mut PluginHandle, arg: Option<&str>) -> bool;

    /// Called to deinitialize the plugin.
    ///
    /// This is always called immediately prior to Drop::drop.
    fn deinit(&self, _ph: &mut PluginHandle) {
    }
}

/// A hexchat plugin handle
pub struct PluginHandle {
    ph: *mut internals::Ph,
    // Used for registration
    info: PluginInfo,
}

pub struct Word<'a> {
    word: Vec<&'a str>
}

pub struct WordEol<'a> {
    word_eol: Vec<&'a str>
}

/// A safety wrapper that ensures you're working with a valid context.
pub struct EnsureValidContext<'a> {
    ph: &'a mut PluginHandle,
}

/// An status indicator for event callbacks. Indicates whether to continue processing, eat hexchat,
/// eat plugin, or eat all.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Eat {
    do_eat: i32,
}

/// Equivalent to HEXCHAT_EAT_NONE.
pub const EAT_NONE: Eat = Eat { do_eat: 0 };
/// Equivalent to HEXCHAT_EAT_HEXCHAT.
pub const EAT_HEXCHAT: Eat = Eat { do_eat: 1 };
/// Equivalent to HEXCHAT_EAT_PLUGIN.
pub const EAT_PLUGIN: Eat = Eat { do_eat: 2 };
/// Equivalent to HEXCHAT_EAT_ALL.
pub const EAT_ALL: Eat = Eat { do_eat: 1 | 2 };

/// A command hook handle.
pub struct CommandHookHandle {
    ph: *mut internals::Ph,
    hh: *const internals::HexchatHook,
    // this does actually store an Rc<...>, but on the other side of the FFI.
    _f: PhantomData<Rc<CommandHookUd>>,
}

/// A server hook handle.
pub struct ServerHookHandle {
    ph: *mut internals::Ph,
    hh: *const internals::HexchatHook,
    // this does actually store an Rc<...>, but on the other side of the FFI.
    _f: PhantomData<Rc<ServerHookUd>>,
}

/// A print hook handle.
pub struct PrintHookHandle {
    ph: *mut internals::Ph,
    hh: *const internals::HexchatHook,
    // this does actually store an Rc<...>, but on the other side of the FFI.
    _f: PhantomData<Rc<PrintHookUd>>,
}

/// A timer hook handle.
pub struct TimerHookHandle {
    ph: *mut internals::Ph,
    hh: *const internals::HexchatHook,
    // avoids issues
    alive: Rc<Cell<bool>>,
    // this does actually store an Rc<...>, but on the other side of the FFI.
    _f: PhantomData<Rc<TimerHookUd>>,
}

/// A context.
// We don't want this Copy + Clone, as we may want to implement a context invalidation system
// at some point (rather than relying on the hexchat allocator not to allocate a new context where
// a context was free'd).
pub struct Context {
    ctx: *const internals::HexchatContext,
}

// #[derive(Debug)] // doesn't work
pub struct InvalidContextError<F: FnOnce(EnsureValidContext) -> R, R>(F);

/// A hexchat_get_info key.
#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub enum InfoId<'a> {
    /// Returns the away message, or `None` if the user is not away.
    Away,
    /// Returns the current channel name.
    Channel,
    /// Returns the current charset.
    Charset,
    /// Returns the hexchat configuration directory, e.g. `/home/user/.config/hexchat`.
    Configdir,
    /// Returns the text event format string for the given text event name.
    EventText(&'a str),
    /// Returns the (real) hostname of the current server.
    Host,
    /// Returns the contents of the input box.
    Inputbox,
    /// Returns the library directory, e.g. `/usr/lib/hexchat`.
    ///
    /// May not always work, as this string isn't necessarily UTF-8, but local file system
    /// encoding.
    Libdirfs,
    /// Returns the channel modes, if known, or `None`.
    Modes,
    /// Returns the current network name, or `None`.
    Network,
    /// Returns the user's current nick.
    Nick,
    /// Returns the user's nickserv password, if any, or `None`
    Nickserv,
    /// Returns the current server name, or `None` if you are not connected.
    Server,
    /// Returns the current channel topic.
    Topic,
    /// Returns the HexChat version string.
    Version,
    /// Returns the window status: "active", "hidden" or "normal".
    WinStatus,
}

// ***** //
// impls //
// ***** //

impl<'a> InfoId<'a> {
    pub fn name(&self) -> Cow<'static, str> {
        match *self {
            InfoId::EventText(s) => {
                let mut eventtext: String = "event_text ".into();
                eventtext.push_str(&s);
                eventtext.into()
            },
            InfoId::Away      => "away".into(),
            InfoId::Channel   => "channel".into(),
            InfoId::Charset   => "charset".into(),
            InfoId::Configdir => "configdir".into(),
            InfoId::Host      => "host".into(),
            InfoId::Inputbox  => "inputbox".into(),
            InfoId::Libdirfs  => "libdirfs".into(),
            InfoId::Modes     => "modes".into(),
            InfoId::Network   => "network".into(),
            InfoId::Nick      => "nick".into(),
            InfoId::Nickserv  => "nickserv".into(),
            InfoId::Server    => "server".into(),
            InfoId::Topic     => "topic".into(),
            InfoId::Version   => "version".into(),
            InfoId::WinStatus => "win_status".into(),
        }
    }
}

impl<F: FnOnce(EnsureValidContext) -> R, R> InvalidContextError<F, R> {
    pub fn get_closure(self) -> F {
        self.0
    }
}

impl Drop for CommandHookHandle {
    fn drop(&mut self) {
        unsafe {
            let b = ((*self.ph).hexchat_unhook)(self.ph, self.hh) as *mut CommandHookUd;
            // we assume b is not null. this should be safe.
            // just drop it
            mem::drop(Rc::from_raw(b));
        }
    }
}
impl Drop for ServerHookHandle {
    fn drop(&mut self) {
        unsafe {
            let b = ((*self.ph).hexchat_unhook)(self.ph, self.hh) as *mut ServerHookUd;
            // we assume b is not null. this should be safe.
            // just drop it
            mem::drop(Rc::from_raw(b));
        }
    }
}
impl Drop for PrintHookHandle {
    fn drop(&mut self) {
        unsafe {
            let b = ((*self.ph).hexchat_unhook)(self.ph, self.hh) as *mut PrintHookUd;
            // we assume b is not null. this should be safe.
            // just drop it
            mem::drop(Rc::from_raw(b));
        }
    }
}
impl Drop for TimerHookHandle {
    fn drop(&mut self) {
        if !self.alive.get() {
            // already free'd.
            return;
        }
        self.alive.set(false);
        unsafe {
            let b = ((*self.ph).hexchat_unhook)(self.ph, self.hh) as *mut TimerHookUd;
            // we assume b is not null. this should be safe.
            // just drop it
            mem::drop(Rc::from_raw(b));
        }
    }
}

impl<'a> Word<'a> {
    unsafe fn new(word: *const *const libc::c_char) -> Word<'a> {
        let mut vec = vec![];
        for i in 1..32 {
            let w = *word.offset(i);
            if !w.is_null() {
                vec.push(CStr::from_ptr(w).to_str().expect("non-utf8 word - broken hexchat"))
            }
        }
        while let Some(&"") = vec.last() {
            vec.pop();
        }
        Word { word: vec }
    }
}

impl<'a> ops::Deref for Word<'a> {
    type Target = [&'a str];
    fn deref(&self) -> &[&'a str] {
        &self.word
    }
}

impl<'a> WordEol<'a> {
    unsafe fn new(word_eol: *const *const libc::c_char) -> WordEol<'a> {
        let mut vec = vec![];
        for i in 1..32 {
            let w = *word_eol.offset(i);
            if !w.is_null() {
                vec.push(CStr::from_ptr(w).to_str().expect("non-utf8 word_eol - broken hexchat"))
            }
        }
        while let Some(&"") = vec.last() {
            vec.pop();
        }
        WordEol { word_eol: vec }
    }
}

impl<'a> ops::Deref for WordEol<'a> {
    type Target = [&'a str];
    fn deref(&self) -> &[&'a str] {
        &self.word_eol
    }
}

impl PluginHandle {
    fn new(ph: *mut internals::Ph, info: PluginInfo) -> PluginHandle {
        PluginHandle {
            ph, info
        }
    }

    /// Registers a hexchat plugin.
    pub fn register(&mut self, name: &str, desc: &str, ver: &str) {
        unsafe {
            let info = self.info;
            if !(*info.name).is_null() || !(*info.desc).is_null() || !(*info.vers).is_null() {
                panic!("Attempt to re-register a plugin");
            }
            let name = CString::new(name).unwrap();
            let desc = CString::new(desc).unwrap();
            let ver = CString::new(ver).unwrap();
            // these shouldn't panic. if they do, we'll need to free them afterwards.
            (*info.name) = name.into_raw();
            (*info.desc) = desc.into_raw();
            (*info.vers) = ver.into_raw();
        }
    }

    /// Ensures the current context is valid.
    ///
    /// # Panics
    ///
    /// This function may panic if it's called while hexchat is closing.
    // NOTE: using a closure is nicer.
    // TODO check if this is actually safe
    pub fn ensure_valid_context<F, R>(&mut self, f: F) -> R where F: FnOnce(EnsureValidContext) -> R {
        // let ctx = self.get_context();
        // if !self.set_context(ctx) {
        //     // invalid context
        //     // TODO fix up the context
        //     unimplemented!()
        // }
        // f(EnsureValidContext { ph: self })
        let ctx = self.get_context();
        // need this here because we don't have NLL yet
        let res = self.with_context(&ctx, f);
        match res {
            Result::Ok(r @ _) => r,
            Result::Err(e @ _) => {
                let nctx = self.find_valid_context().expect("ensure_valid_context failed (find_valid_context failed), was hexchat closing?");
                self.with_context(&nctx, e.get_closure()).ok().expect("ensure_valid_context failed, was hexchat closing?")
            }
        }
    }

    /// Returns the current context.
    ///
    /// Note: The returned context may be invalid. Use [`set_context`] to check.
    ///
    /// [`set_context`]: #method.set_context
    pub fn get_context(&mut self) -> Context {
        unsafe {
            Context { ctx: ((*self.ph).hexchat_get_context)(self.ph) }
        }
    }

    /// Sets the current context.
    ///
    /// Returns `true` if the context is valid, `false` otherwise.
    pub fn set_context(&mut self, ctx: &Context) -> bool {
        unsafe {
            ((*self.ph).hexchat_set_context)(self.ph, ctx.ctx) != 0
        }
    }

    /// Do something in a valid context.
    ///
    /// Note that this changes the active context and doesn't change it back.
    ///
    /// # Errors
    ///
    /// Returns `Err(InvalidContextError(f))` if the context is invalid. See [`set_context`]. Otherwise,
    /// calls `f` and returns `Ok(its result)`.
    ///
    /// Note that `InvalidContextError` contains the original closure. This allows you to retry.
    ///
    /// [`set_context`]: #method.set_context
    // this is probably safe to inline, and actually a good idea for ensure_valid_context
    #[inline]
    pub fn with_context<F, R>(&mut self, ctx: &Context, f: F) -> Result<R, InvalidContextError<F, R>> where F: FnOnce(EnsureValidContext) -> R {
        if !self.set_context(ctx) {
            Err(InvalidContextError(f))
        } else {
            Ok(f(EnsureValidContext { ph: self }))
        }
    }

    /// Sets a command hook
    pub fn hook_command<F>(&mut self, cmd: &str, cb: F, pri: i32, help: Option<&str>) -> CommandHookHandle where F: Fn(&mut PluginHandle, Word, WordEol) -> Eat + 'static + ::std::panic::RefUnwindSafe {
        unsafe extern "C" fn callback(word: *const *const libc::c_char, word_eol: *const *const libc::c_char, ud: *mut libc::c_void) -> libc::c_int {
            // hook may unhook itself.
            // however, we don't wanna free it until it has returned.
            let f: Rc<CommandHookUd> = rc_clone_from_raw(ud as *const CommandHookUd);
            let ph = f.1;
            match catch_unwind(move || {
                let word = Word::new(word);
                let word_eol = WordEol::new(word_eol);
                (f.0)(&mut PluginHandle::new(f.1, f.2), word, word_eol).do_eat as libc::c_int
            }) {
                Result::Ok(v @ _) => v,
                Result::Err(e @ _) => {
                    // if it's a &str or String, just print it
                    if let Some(estr) = e.downcast_ref::<&str>() {
                        hexchat_print_str(ph, estr, false);
                    } else if let Some(estring) = e.downcast_ref::<String>() {
                        hexchat_print_str(ph, &estring, false);
                    }
                    0 // EAT_NONE
                }
            }
        }
        let b: Rc<CommandHookUd> = Rc::new((Box::new(cb), self.ph, self.info));
        let name = CString::new(cmd).unwrap();
        let help_text = help.map(CString::new).map(Result::unwrap);
        let bp = Rc::into_raw(b);
        unsafe {
            let res = ((*self.ph).hexchat_hook_command)(self.ph, name.as_ptr(), pri as libc::c_int, callback, help_text.as_ref().map(|s| s.as_ptr()).unwrap_or(ptr::null()), bp as *mut _);
            assert!(!res.is_null());
            CommandHookHandle { ph: self.ph, hh: res, _f: PhantomData }
        }
    }
    /// Sets a server hook
    pub fn hook_server<F>(&mut self, cmd: &str, cb: F, pri: i32) -> ServerHookHandle where F: Fn(&mut PluginHandle, Word, WordEol) -> Eat + 'static + ::std::panic::RefUnwindSafe {
        unsafe extern "C" fn callback(word: *const *const libc::c_char, word_eol: *const *const libc::c_char, ud: *mut libc::c_void) -> libc::c_int {
            // hook may unhook itself.
            // however, we don't wanna free it until it has returned.
            let f: Rc<ServerHookUd> = rc_clone_from_raw(ud as *const ServerHookUd);
            let ph = f.1;
            match catch_unwind(move || {
                let word = Word::new(word);
                let word_eol = WordEol::new(word_eol);
                (f.0)(&mut PluginHandle::new(f.1, f.2), word, word_eol).do_eat as libc::c_int
            }) {
                Result::Ok(v @ _) => v,
                Result::Err(e @ _) => {
                    // if it's a &str or String, just print it
                    if let Some(estr) = e.downcast_ref::<&str>() {
                        hexchat_print_str(ph, estr, false);
                    } else if let Some(estring) = e.downcast_ref::<String>() {
                        hexchat_print_str(ph, &estring, false);
                    }
                    0 // EAT_NONE
                }
            }
        }
        let b: Rc<ServerHookUd> = Rc::new((Box::new(cb), self.ph, self.info));
        let name = CString::new(cmd).unwrap();
        let bp = Rc::into_raw(b);
        unsafe {
            let res = ((*self.ph).hexchat_hook_server)(self.ph, name.as_ptr(), pri as libc::c_int, callback, bp as *mut _);
            assert!(!res.is_null());
            ServerHookHandle { ph: self.ph, hh: res, _f: PhantomData }
        }
    }
    /// Sets a print hook
    pub fn hook_print<F>(&mut self, name: &str, cb: F, pri: i32) -> PrintHookHandle where F: Fn(&mut PluginHandle, Word) -> Eat + 'static + ::std::panic::RefUnwindSafe {
        unsafe extern "C" fn callback(word: *const *const libc::c_char, ud: *mut libc::c_void) -> libc::c_int {
            // hook may unhook itself.
            // however, we don't wanna free it until it has returned.
            let f: Rc<PrintHookUd> = rc_clone_from_raw(ud as *const PrintHookUd);
            let ph = f.1;
            match catch_unwind(move || {
                let word = Word::new(word);
                (f.0)(&mut PluginHandle::new(f.1, f.2), word).do_eat as libc::c_int
            }) {
                Result::Ok(v @ _) => v,
                Result::Err(e @ _) => {
                    // if it's a &str or String, just print it
                    if let Some(estr) = e.downcast_ref::<&str>() {
                        hexchat_print_str(ph, estr, false);
                    } else if let Some(estring) = e.downcast_ref::<String>() {
                        hexchat_print_str(ph, &estring, false);
                    }
                    0 // EAT_NONE
                }
            }
        }
        let b: Rc<PrintHookUd> = Rc::new((Box::new(cb), self.ph, self.info));
        let name = CString::new(name).unwrap();
        let bp = Rc::into_raw(b);
        unsafe {
            let res = ((*self.ph).hexchat_hook_print)(self.ph, name.as_ptr(), pri as libc::c_int, callback, bp as *mut _);
            assert!(!res.is_null());
            PrintHookHandle { ph: self.ph, hh: res, _f: PhantomData }
        }
    }
    /// Sets a timer hook
    pub fn hook_timer<F>(&mut self, timeout: i32, cb: F) -> TimerHookHandle where F: Fn(&mut PluginHandle) -> bool + 'static + ::std::panic::RefUnwindSafe {
        unsafe extern "C" fn callback(ud: *mut libc::c_void) -> libc::c_int {
            // hook may unhook itself.
            // however, we don't wanna free it until it has returned.
            let f: Rc<TimerHookUd> = rc_clone_from_raw(ud as *const TimerHookUd);
            let alive = f.1.clone(); // clone the alive because why not
            let f = f.0.clone();
            let ph = f.1;
            // we could technically free the Rc<TimerHookUd> here, I guess
            match catch_unwind(move || {
                (f.0)(&mut PluginHandle::new(f.1, f.2))
            }) {
                Result::Ok(true) => 1,
                Result::Ok(false) => {
                    // avoid double-free
                    if !alive.get() {
                        return 0;
                    }
                    // mark it no longer alive
                    alive.set(false);
                    // HexChat will automatically free the hook.
                    // we just need to free the userdata.
                    mem::drop(Rc::from_raw(ud as *const TimerHookUd));
                    0
                },
                Result::Err(e @ _) => {
                    // if it's a &str or String, just print it
                    if let Some(estr) = e.downcast_ref::<&str>() {
                        hexchat_print_str(ph, estr, false);
                    } else if let Some(estring) = e.downcast_ref::<String>() {
                        hexchat_print_str(ph, &estring, false);
                    }
                    // avoid double-free
                    if !alive.get() {
                        return 0;
                    }
                    // mark it no longer alive
                    alive.set(false);
                    // HexChat will automatically free the hook.
                    // we just need to free the userdata.
                    mem::drop(Rc::from_raw(ud as *const TimerHookUd));
                    0
                }
            }
        }
        let alive = Rc::new(Cell::new(true));
        let b: Rc<TimerHookUd> = Rc::new((Rc::new((Box::new(cb), self.ph, self.info)), alive.clone()));
        let bp = Rc::into_raw(b);
        unsafe {
            let res = ((*self.ph).hexchat_hook_timer)(self.ph, timeout as libc::c_int, callback, bp as *mut _);
            assert!(!res.is_null());
            TimerHookHandle { ph: self.ph, hh: res, alive, _f: PhantomData }
        }
    }

    /// Prints to the hexchat buffer.
    // this checks the context internally. if it didn't, it wouldn't be safe to have here.
    pub fn print(&mut self, s: &str) {
        unsafe {
            hexchat_print_str(self.ph, s, true);
        }
    }

    /// Returns information on the current context.
    ///
    /// Note: `InfoId::Libdirfs` may return `None` or broken results if the result wouldn't be (valid) UTF-8.
    pub fn get_info(&mut self, id: &InfoId) -> Option<String> {
        let ph = self.ph;
        let id_cstring = CString::new(&*id.name()).unwrap();
        unsafe {
            let res = ((*ph).hexchat_get_info)(ph, id_cstring.as_ptr());
            if res.is_null() {
                None
            } else {
                let s = CStr::from_ptr(res).to_owned().into_string();
                if *id != InfoId::Libdirfs {
                    Some(s.expect("non-utf8 word - broken hexchat"))
                } else {
                    s.ok()
                }
            }
        }
    }

    // ******* //
    // PRIVATE //
    // ******* //

    fn find_valid_context(&mut self) -> Option<Context> {
        unsafe {
            let ph = self.ph;
            // TODO wrap this in a safer API, with proper Drop
            #[allow(unused_mut)]
            let mut list = ((*ph).hexchat_list_get)(ph, cstr(b"channels\0"));
            // hexchat does this thing where it puts a context in a list_str.
            // this is the proper way to do this
            if ((*ph).hexchat_list_next)(ph, list) != 0 {
                // if this panics we may leak some memory. it's not a big deal tho, as it indicates
                // a bug in hexchat-plugin.rs.
                let ctx = ((*ph).hexchat_list_str)(ph, list, cstr(b"context\0")) as *const internals::HexchatContext;
                ((*ph).hexchat_list_free)(ph, list);
                Some(Context { ctx })
            } else {
                ((*ph).hexchat_list_free)(ph, list);
                None
            }
        }
    }
}

impl<'a> EnsureValidContext<'a> {
/*
 * These cause UB:
 * `hexchat_command` may invalidate the plugin context.
 * `hexchat_find_context` and `hexchat_nickcmp` use the plugin context without checking it.
 * `hexchat_get_prefs` uses the plugin context if name == "state_cursor" or "id" (or anything with
 * the same hash).
 * `hexchat_list_get` uses the plugin context if name == "notify" (or anything with the same hash).
 * `hexchat_list_str`, `hexchat_list_int`, 
 * `hexchat_emit_print`, `hexchat_emit_print_attrs` use the plugin context.
 * `hexchat_send_modes` uses the plugin context.
 * We need to wrap them (or, alternatively, hexchat_command). However, there's no (safe) way to get
 * a valid context afterwards.
 * - Actually that's a lie. Hexchat does a trick to give you a context as part of the channel list.
 *     We can use that to our advantage. I'm not sure if it's better to wrap hexchat_command or the
 *     other functions, tho.
 *     (Do we want to walk a linked list every time we use hexchat_command? I'd think
 *     hexchat_command is the most used API function... Plus, emit_print could indirectly
 *     invalidate the context as well.)
 *
 * For performance we put them behind an EnsureValidContext - things that don't invalidate the
 * context take an `&mut self`, things that do take an `self`.
 */

    /// Finds an open context for the given servname and channel.
    pub fn find_context(&mut self, servname: Option<&str>, channel: Option<&str>) -> Option<Context> {
        // this was a mistake but oh well
        let ph = self.ph.ph;
        let servname = servname.map(|x| CString::new(x).unwrap());
        let channel = channel.map(|x| CString::new(x).unwrap());
        let ctx = unsafe {
            let sptr = servname.map(|x| x.as_ptr()).unwrap_or(ptr::null());
            let cptr = channel.map(|x| x.as_ptr()).unwrap_or(ptr::null());
            ((*ph).hexchat_find_context)(ph, sptr, cptr)
        };
        if ctx.is_null() {
            None
        } else {
            Some(Context { ctx })
        }
    }

    /// Compares two nicks based on the server's case mapping.
    pub fn nickcmp(&mut self, nick1: &str, nick2: &str) -> ::std::cmp::Ordering {
        use std::cmp::Ordering;
        // this was a mistake but oh well
        let ph = self.ph.ph;
        // need to put this in a more permanent position than temporaries
        let nick1 = CString::new(nick1).unwrap();
        let nick2 = CString::new(nick2).unwrap();
        let res = unsafe {
            ((*ph).hexchat_nickcmp)(ph, nick1.as_ptr(), nick2.as_ptr())
        };
        if res < 0 {
            Ordering::Less
        } else if res > 0 {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }

    pub fn send_modes<'b, I: IntoIterator<Item=&'b str>>(&mut self, iter: I, mpl: i32, sign: char, mode: char) {
        // this was a mistake but oh well
        let ph = self.ph.ph;
        assert!(sign == '+' || sign == '-', "sign must be + or -");
        assert!(mode.is_ascii(), "mode must be ascii");
        assert!(mpl >= 0, "mpl must be non-negative");
        let v: Vec<CString> = iter.into_iter().map(|s| CString::new(s).unwrap()).collect();
        let mut v2: Vec<*const libc::c_char> = (&v).iter().map(|x| x.as_ptr()).collect();
        let arr: &mut [*const libc::c_char] = &mut *v2;
        unsafe {
            ((*ph).hexchat_send_modes)(ph, arr.as_mut_ptr(), arr.len() as libc::c_int,
                mpl as libc::c_int, sign as libc::c_char, mode as libc::c_char)
        }
    }

    /// Executes a command.
    pub fn command(self, cmd: &str) {
        // this was a mistake but oh well
        let ph = self.ph.ph;
        // need to put this in a more permanent position than temporaries
        let cmd = CString::new(cmd).unwrap();
        unsafe {
            ((*ph).hexchat_command)(ph, cmd.as_ptr())
        }
    }

    pub fn emit_print(self) {
        // TODO
        unimplemented!()
    }

    pub fn emit_print_attrs(self) {
        // TODO
        unimplemented!()
    }

    // ******** //
    // FORWARDS //
    // ******** //

    pub fn get_context(&mut self) -> Context {
        self.ph.get_context()
    }

    /// Sets the current context.
    ///
    /// Returns `true` if the context is valid, `false` otherwise.
    pub fn set_context(&mut self, ctx: &Context) -> bool {
        self.ph.set_context(ctx)
    }

    /// Prints to the hexchat buffer.
    // as of hexchat 2.14.1, this does not call hooks.
    pub fn print(&mut self, s: &str) {
        self.ph.print(s)
    }

    /// Sets a command hook
    pub fn hook_command<F>(&mut self, cmd: &str, cb: F, pri: i32, help: Option<&str>) -> CommandHookHandle where F: Fn(&mut PluginHandle, Word, WordEol) -> Eat + 'static + ::std::panic::RefUnwindSafe {
        self.ph.hook_command(cmd, cb, pri, help)
    }
    /// Sets a server hook
    pub fn hook_server<F>(&mut self, cmd: &str, cb: F, pri: i32) -> ServerHookHandle where F: Fn(&mut PluginHandle, Word, WordEol) -> Eat + 'static + ::std::panic::RefUnwindSafe {
        self.ph.hook_server(cmd, cb, pri)
    }
    /// Sets a print hook
    pub fn hook_print<F>(&mut self, name: &str, cb: F, pri: i32) -> PrintHookHandle where F: Fn(&mut PluginHandle, Word) -> Eat + 'static + ::std::panic::RefUnwindSafe {
        self.ph.hook_print(name, cb, pri)
    }
    /// Sets a timer hook
    pub fn hook_timer<F>(&mut self, timeout: i32, cb: F) -> TimerHookHandle where F: Fn(&mut PluginHandle) -> bool + 'static + ::std::panic::RefUnwindSafe {
        self.ph.hook_timer(timeout, cb)
    }
    pub fn get_info(&mut self, id: &InfoId) -> Option<String> {
        self.ph.get_info(id)
    }
}

// ******* //
// PRIVATE //
// ******* //

// Type aliases, used for safety type checking.
/// Userdata type used by a command hook.
// We actually do want RefUnwindSafe. This function may be called multiple times, and it's not
// automatically invalidated if it panics, so it may be called again after it panics. If you need
// mutable state, std provides std::sync::Mutex which has poisoning. Other interior mutability with
// poisoning could also be used. std doesn't have anything for single-threaded performance (yet),
// but hexchat isn't particularly performance-critical.
type CommandHookUd = (Box<Fn(&mut PluginHandle, Word, WordEol) -> Eat + ::std::panic::RefUnwindSafe>, *mut internals::Ph, PluginInfo);
/// Userdata type used by a server hook.
type ServerHookUd = (Box<Fn(&mut PluginHandle, Word, WordEol) -> Eat + ::std::panic::RefUnwindSafe>, *mut internals::Ph, PluginInfo);
/// Userdata type used by a print hook.
type PrintHookUd = (Box<Fn(&mut PluginHandle, Word) -> Eat + ::std::panic::RefUnwindSafe>, *mut internals::Ph, PluginInfo);
/// Userdata type used by a timer hook.
type TimerHookUd = (Rc<(Box<Fn(&mut PluginHandle) -> bool + ::std::panic::RefUnwindSafe>, *mut internals::Ph, PluginInfo)>, Rc<Cell<bool>>);

/// The contents of an empty CStr.
///
/// This is useful where you need a non-null CStr.
// NOTE: MUST BE b"\0"!
const EMPTY_CSTRING_DATA: &[u8] = b"\0";

/// Converts a nul-terminated const bstring to a C string.
///
/// # Panics
///
/// Panics if b contains embedded nuls.
// TODO const fn, once that's possible
fn cstr(b: &'static [u8]) -> *const libc::c_char {
    CStr::from_bytes_with_nul(b).unwrap().as_ptr()
}

/// Clones an Rc straight from a raw pointer.
///
/// # Safety
///
/// This function is unsafe because `ptr` must hame come from `Rc::into_raw`.
unsafe fn rc_clone_from_raw<T>(ptr: *const T) -> Rc<T> {
    // this is a bit confusing to read, but basically, we get an Rc from the raw ptr, and increment
    // the refcount.
    // The construct mem::forget(rc.clone()) increments the refcount.
    let rc = Rc::from_raw(ptr);
    mem::forget(rc.clone());
    rc
}

/// Prints an &str to hexchat, trying to avoid allocations.
///
/// # Safety
///
/// This function does not check the passed in argument.
///
/// # Panics
///
/// Panics if panic_on_nul is true and the string contains embedded nuls.
unsafe fn hexchat_print_str(ph: *mut internals::Ph, s: &str, panic_on_nul: bool) {
    match CString::new(s) {
        Result::Ok(cs @ _) => {
            let csr: &CStr = &cs;
            ((*ph).hexchat_print)(ph, csr.as_ptr())
        },
        e @ _ => if panic_on_nul {e.unwrap();}, // TODO nul_position?
    }
}

/// Holds name, desc, vers
// This is kinda naughty - we modify these values after returning from hexchat_plugin_init, during
// the deinitialization.
// However, if my reading of the HexChat code is correct, this is "ok".
#[derive(Copy, Clone)]
struct PluginInfo {
    name: *mut *const libc::c_char,
    desc: *mut *const libc::c_char,
    vers: *mut *const libc::c_char,
}

impl PluginInfo {
    /// Creates a PluginInfo.
    ///
    /// # Panics
    ///
    /// This function explicitly doesn't panic. Call unwrap() on the result instead.
    fn new(name: *mut *const libc::c_char, desc: *mut *const libc::c_char, vers: *mut *const libc::c_char) -> Option<PluginInfo> {
        if name.is_null() || desc.is_null() || vers.is_null() || name == desc || desc == vers || name == vers {
            None
        } else {
            Some(unsafe { PluginInfo::new_unchecked(name, desc, vers) })
        }
    }

    /// Creates a PluginInfo without checking the arguments.
    ///
    /// # Safety
    ///
    /// This function is unsafe, as it doesn't check the validity of the arguments. You're expected
    /// to only pass in non-aliased non-null pointers. Use new if unsure.
    unsafe fn new_unchecked(name: *mut *const libc::c_char, desc: *mut *const libc::c_char, vers: *mut *const libc::c_char) -> PluginInfo {
        PluginInfo {
            name, desc, vers
        }
    }

    /// Drop relevant CStrings.
    ///
    /// # Safety
    ///
    /// This function is unsafe because calling it may trigger Undefined Behaviour. See also
    /// [CString::from_raw].
    ///
    /// [from_raw]: https://doc.rust-lang.org/std/ffi/struct.CString.html#method.from_raw
    unsafe fn drop_info(&mut self) {
        if !(*self.name).is_null() {
            mem::drop(CString::from_raw(*self.name as *mut _));
            *self.name = cstr(EMPTY_CSTRING_DATA);
        }
        if !(*self.desc).is_null() {
            mem::drop(CString::from_raw(*self.desc as *mut _));
            *self.desc = cstr(EMPTY_CSTRING_DATA);
        }
        if !(*self.vers).is_null() {
            mem::drop(CString::from_raw(*self.vers as *mut _));
            *self.vers = cstr(EMPTY_CSTRING_DATA);
        }
    }
}

/// Plugin data stored in the hexchat plugin_handle.
struct PhUserdata {
    plug: Box<Plugin>,
    pluginfo: PluginInfo,
}

/// Puts the userdata in the plugin handle.
///
/// # Safety
///
/// This function is unsafe because it doesn't check if the pointer is valid.
///
/// Improper use of this function can leak memory.
unsafe fn put_userdata(ph: *mut internals::Ph, ud: Box<PhUserdata>) {
    (*ph).userdata = Box::into_raw(ud) as *mut libc::c_void;
}

// unsafe fn get_userdata(ph: *mut internals::Ph) -> *const PhUserdata {
//     (*ph).userdata as *const _
// }

/// Pops the userdata from the plugin handle.
///
/// # Safety
///
/// This function is unsafe because it doesn't check if the pointer is valid.
unsafe fn pop_userdata(ph: *mut internals::Ph) -> Box<PhUserdata> {
    Box::from_raw(mem::replace(&mut (*ph).userdata, ptr::null_mut()) as *mut PhUserdata)
}

// *********************** //
// PUBLIC OUT OF NECESSITY //
// *********************** //

#[doc(hidden)]
pub unsafe fn hexchat_plugin_init<T>(plugin_handle: *mut libc::c_void,
                                     plugin_name: *mut *const libc::c_char,
                                     plugin_desc: *mut *const libc::c_char,
                                     plugin_version: *mut *const libc::c_char,
                                     arg: *const libc::c_char) -> libc::c_int
                                     where T: Plugin + Default + 'static {
    if plugin_handle.is_null() || plugin_name.is_null() || plugin_desc.is_null() || plugin_version.is_null() {
        // we can't really do anything here.
        eprintln!("hexchat_plugin_init called with a null pointer that shouldn't be null - broken hexchat");
        // TODO maybe call abort.
        return 0;
    }
    let ph = plugin_handle as *mut internals::Ph;
    // clear the "userdata" field first thing - if the deinit function gets called (wrong hexchat
    // version, other issues), we don't wanna try to drop the hexchat_dummy or hexchat_read_fd
    // function as if it were a Box!
    (*ph).userdata = ptr::null_mut();
    // read the filename so we can pass it on later.
    let filename = if !(*plugin_name).is_null() {
        if let Ok(fname) = CStr::from_ptr(*plugin_name).to_owned().into_string() {
            fname
        } else {
            eprintln!("failed to convert filename to utf8 - broken hexchat");
            return 0;
        }
    } else {
        // no filename specified for some reason, but we can still load
        String::new() // empty string
    };
    // these may be null, unless initialization is successful.
    // we set them to null as markers.
    *plugin_name = ptr::null();
    *plugin_desc = ptr::null();
    *plugin_version = ptr::null();
    // do some version checks for safety
    // NOTE: calling hexchat functions with null plugin_name, plugin_desc, plugin_version is a bit
    // dangerous. this particular case is "ok".
    {
        let ver = ((*ph).hexchat_get_info)(ph, cstr(b"version\0")); // this shouldn't panic
        let cstr = CStr::from_ptr(ver);
        if let Ok(ver) = cstr.to_str() {
            let mut iter = ver.split('.');
            let a = iter.next().map(i32::from_str).and_then(Result::ok).unwrap_or(0);
            let b = iter.next().map(i32::from_str).and_then(Result::ok).unwrap_or(0);
            let c = iter.next().map(i32::from_str).and_then(Result::ok).unwrap_or(0);
            // 2.9.6 or greater
            if !(a > 2 || (a == 2 && (b > 9 || (b == 9 && (c > 6 || (c == 6)))))) {
                return 0;
            }
        } else {
            return 0;
        }
    }
    let mut pluginfo = if let Some(pluginfo) = PluginInfo::new(plugin_name, plugin_desc, plugin_version) {
        pluginfo
    } else {
        return 0;
    };
    let r: thread::Result<Option<Box<_>>> = {
        catch_unwind(move || {
            let mut pluginhandle = PluginHandle {
                ph: ph,
                info: pluginfo,
            };
            let plug = T::default();
            if plug.init(&mut pluginhandle, if !arg.is_null() { Some(CStr::from_ptr(arg).to_str().expect("arg not valid utf-8 - broken hexchat")) } else { None }) {
                if !(pluginfo.name.is_null() || pluginfo.desc.is_null() || pluginfo.vers.is_null()) {
                    Some(Box::new(PhUserdata { plug: Box::new(plug), pluginfo }))
                } else {
                    // TODO log: forgot to call register
                    None
                }
            } else {
                None
            }
        })
    };
    match r {
        Result::Ok(Option::Some(plug @ _)) => {
            if (*plugin_name).is_null() || (*plugin_desc).is_null() || (*plugin_version).is_null() {
                // TODO deallocate any which are non-null
                pluginfo.drop_info();
                0
            } else {
                put_userdata(ph, plug);
                1
            }
        },
        r @ _ => {
            // if the initialization fails, deinit doesn't get called, so we need to clean up
            // ourselves.
            
            if let Err(_) = r {
                // TODO try to log panic?
            }
            0
        },
    }
}

#[doc(hidden)]
pub unsafe fn hexchat_plugin_deinit<T>(plugin_handle: *mut libc::c_void) where T: Plugin {
    // plugin_handle should never be null, but just in case.
    if !plugin_handle.is_null() {
        let ph = plugin_handle as *mut internals::Ph;
        // userdata should also never be null.
        if !(*ph).userdata.is_null() {
            {
                let mut info: Option<PluginInfo> = None;
                {
                    let mut ausinfo = ::std::panic::AssertUnwindSafe(&mut info);
                    catch_unwind(move || {
                        let userdata = *pop_userdata(ph);
                        **ausinfo = Some(userdata.pluginfo);
                        userdata.plug.deinit(&mut PluginHandle { ph, info: userdata.pluginfo });
                    }).ok();
                }
                if let Some(mut info) = info {
                    info.drop_info();
                } else {
                    eprintln!("I have no idea tbh, I didn't know `pop_userdata` could panic!");
                }
            }
        } else {
            eprintln!("null userdata in hexchat_plugin_deinit - broken hexchat or broken hexchat-plugin.rs");
        }
    } else {
        eprintln!("hexchat_plugin_deinit called with a null plugin_handle - broken hexchat");
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
