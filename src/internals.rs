/*
 * Hexchat Plugin API Bindings for Rust - Internals
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
//! Implementation details, mostly.
//!
//! This also includes the hexchat_plugin struct, from hexchat-plugin.h. Note that we use the
//! struct even on non-Windows platforms because it's a lot easier that way. Should be safe, tho.

use libc;

// apparently this is the right way to do these
#[repr(i8)]
pub enum HexchatList {
    __One,
    __Two,
}
#[repr(i8)]
pub enum HexchatHook {
    __One,
    __Two,
}
#[repr(i8)]
pub enum HexchatContext {
    __One,
    __Two,
}

// not in hexchat-plugin.h
#[repr(i8)]
pub enum PluginGuiHandle {
    __One,
    __Two,
}

#[repr(C)]
pub struct HexchatEventAttrs {
    server_time_utc: libc::time_t,
}

pub type HexchatPlugin = Ph;

#[repr(C)]
pub struct Ph {
    pub hexchat_hook_command: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            name: *const libc::c_char,
            pri: libc::c_int,
            /* CALLBACK */
            callback: unsafe extern "C" fn(word: *const *const libc::c_char, word_eol: *const *const libc::c_char, user_data: *mut libc::c_void) -> libc::c_int,
            help_text: *const libc::c_char,
            userdata: *mut libc::c_void) -> *const HexchatHook,
    pub hexchat_hook_server: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            name: *const libc::c_char,
            pri: libc::c_int,
            /* CALLBACK */
            callback: Option<unsafe extern "C" fn(word: *const *const libc::c_char, word_eol: *const *const libc::c_char, user_data: *mut libc::c_void) -> libc::c_int>,
            userdata: *mut libc::c_void) -> *const HexchatHook,
    pub hexchat_hook_print: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            name: *const libc::c_char,
            pri: libc::c_int,
            /* CALLBACK */
            callback: Option<unsafe extern "C" fn(word: *const *const libc::c_char, user_data: *mut libc::c_void) -> libc::c_int>,
            userdata: *mut libc::c_void) -> *const HexchatHook,
    pub hexchat_hook_timer: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            timeout: libc::c_int,
            /* CALLBACK */
            callback: Option<unsafe extern "C" fn(user_data: *mut libc::c_void) -> libc::c_int>,
            userdata: *mut libc::c_void) -> *const HexchatHook,
    pub hexchat_hook_fd: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            fd: libc::c_int,
            flags: libc::c_int,
            /* CALLBACK */
            callback: Option<unsafe extern "C" fn(fd: libc::c_int, flags: libc::c_int, user_data: *mut libc::c_void) -> libc::c_int>,
            userdata: *mut libc::c_void) -> *const HexchatHook,
    pub hexchat_unhook: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            hook: *const HexchatHook) -> *const libc::c_void,
    pub hexchat_print: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            text: *const libc::c_char),
    pub hexchat_printf_do_not_use: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            format: *const libc::c_char, ...),
    pub hexchat_command: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            command: *const libc::c_char),
    pub hexchat_commandf_do_not_use: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            format: *const libc::c_char, ...),
    pub hexchat_nickcmp: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            s1: *const libc::c_char,
            s2: *const libc::c_char) -> libc::c_int,
    pub hexchat_set_context: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            ctx: *const HexchatContext) -> libc::c_int,
    pub hexchat_find_context: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            servname: *const libc::c_char,
            channel: *const libc::c_char) -> *const HexchatContext,
    pub hexchat_get_context: unsafe extern "C" fn(ph: *mut HexchatPlugin) -> *const HexchatContext,
    pub hexchat_get_info: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            id: *const libc::c_char) -> *const libc::c_char,
    pub hexchat_get_prefs: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            name: *const libc::c_char,
            string: *mut *const libc::c_char,
            integer: *mut libc::c_int) -> libc::c_int,
    pub hexchat_list_get: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            name: *const libc::c_char) -> *const HexchatList,
    pub hexchat_list_free: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            xlist: *const HexchatList),
    pub hexchat_list_fields: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            name: *const libc::c_char) -> *const *const libc::c_char,
    pub hexchat_list_next: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            xlist: *const HexchatList) -> libc::c_int,
    pub hexchat_list_str: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            xlist: *const HexchatList,
            name: *const libc::c_char) -> *const libc::c_char,
    pub hexchat_list_int: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            xlist: *const HexchatList,
            name: *const libc::c_char) -> libc::c_int,
    pub hexchat_plugingui_add: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            filename: *const libc::c_char,
            name: *const libc::c_char,
            desc: *const libc::c_char,
            version: *const libc::c_char,
            reserved: *mut char) -> *const PluginGuiHandle,
    pub hexchat_plugingui_remove: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            handle: *const PluginGuiHandle),
    pub hexchat_emit_print: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            event_name: *const libc::c_char, ...) -> libc::c_int,
    // this is VERY NAUGHTY.
    // TODO see if hexchat's gonna provide a proper userdata field at some point.
    // it appears this function isn't used anywhere by hexchat so we reuse its pointer.
    // on linux, it's a dummy anyway.
    // another option would've been to use one of the printf functions.
    // TODO test this on platforms hexchat doesn't build on, like AVR.
    pub userdata: *mut libc::c_void,
    /*pub hexchat_read_fd: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            src: *const libc::c_void,
            buf: *mut char,
            len: *mut libc::c_int) -> libc::c_int,*/
    pub hexchat_list_time: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            xlist: *const HexchatList,
            name: *const libc::c_char) -> libc::time_t,
    pub hexchat_gettext: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            msgid: *const libc::c_char) -> *const libc::c_char,
    pub hexchat_send_modes: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            targets: *mut *const libc::c_char,
            ntargets: libc::c_int,
            modes_per_line: libc::c_int,
            sign: libc::c_char,
            mode: libc::c_char),
    pub hexchat_strip: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            string: *const libc::c_char,
            len: libc::c_int,
            flags: libc::c_int) -> *const libc::c_char,
    pub hexchat_free: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            ptr: *const libc::c_void),
    pub hexchat_pluginpref_set_str: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            var: *const libc::c_char,
            value: *const libc::c_char) -> libc::c_int,
    pub hexchat_pluginpref_get_str: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            var: *const libc::c_char,
            dest: *mut char) -> libc::c_int,
    pub hexchat_pluginpref_set_int: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            var: *const libc::c_char,
            value: libc::c_int) -> libc::c_int,
    pub hexchat_pluginpref_get_int: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            var: *const libc::c_char) -> libc::c_int,
    pub hexchat_pluginpref_delete: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            var: *const libc::c_char) -> libc::c_int,
    pub hexchat_pluginpref_list: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            dest: *mut char) -> libc::c_int,
    pub hexchat_hook_server_attrs: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            name: *const libc::c_char,
            pri: libc::c_int,
            /* CALLBACK */
            callback: Option<unsafe extern "C" fn(word: *const *const libc::c_char, word_eol: *const *const libc::c_char, attrs: *const HexchatEventAttrs, user_data: *mut libc::c_void) -> libc::c_int>,
            userdata: *mut libc::c_void) -> *const HexchatHook,
    pub hexchat_hook_print_attrs: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            name: *const libc::c_char,
            pri: libc::c_int,
            /* CALLBACK */
            callback: Option<unsafe extern "C" fn(word: *const *const libc::c_char, attrs: *const HexchatEventAttrs, user_data: *mut libc::c_void) -> libc::c_int>,
            userdata: *mut libc::c_void) -> *const HexchatHook,
    pub hexchat_emit_print_attrs: unsafe extern "C" fn(ph: *mut HexchatPlugin, attrs: *const HexchatEventAttrs,
            event_name: *const libc::c_char, ...) -> libc::c_int,
    pub hexchat_event_attrs_create: unsafe extern "C" fn(ph: *mut HexchatPlugin) -> *mut HexchatEventAttrs,
    pub hexchat_event_attrs_free: unsafe extern "C" fn(ph: *mut HexchatPlugin,
            attrs: *const HexchatEventAttrs),
}
