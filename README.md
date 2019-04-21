Rust hexchat-plugin
===================

`hexchat-plugin` provides safe and rusty API bindings for developing native HexChat plugins.

Example plugin:

```rust
#[macro_use]
extern crate hexchat_plugin;

use hexchat_plugin::{Plugin, PluginHandle, InfoId};

use std::sync::Mutex;
use std::sync::Arc;

#[derive(Default)]
struct MyPlug {
    // be careful with these: we don't want to move them into the hooks, as that would cause memory leaks!
    // we can safely use Arc::downgrade on them, however!
    cmutex: Arc<Mutex<Vec<hexchat_plugin::CommandHookHandle>>>,
    smutex: Arc<Mutex<Vec<hexchat_plugin::ServerHookHandle>>>,
    pmutex: Arc<Mutex<Vec<hexchat_plugin::PrintHookHandle>>>,
    tmutex: Arc<Mutex<Vec<hexchat_plugin::TimerHookHandle>>>,
}


impl Plugin for MyPlug {
    fn init(&self, ph: &mut PluginHandle, _arg: Option<&str>) -> bool {
        ph.register("MyPlug", "Prints the old topic on topic change", "0.1.0");
        ph.print("Loaded MyPlug 0.1.0");

        let enabled = Arc::new(Mutex::new(false));
        {
            let flag = enabled.clone();
            self.pmutex.lock().unwrap().push(ph.hook_print("Topic Change", move |ph, _word| {
                if *flag.lock().unwrap() {
                    if let Some(topic) = ph.get_info(&InfoId::Topic) {
                        ph.print(&format!("\x0322*\t\x0329Previous topic:\x03 {}", topic));
                    }
                }
                hexchat_plugin::EAT_NONE
            }, hexchat_plugin::PRI_NORM));
        }

        self.cmutex.lock().unwrap().push(ph.hook_command("PrintOldTopic", move |ph, _word, word_eol| {
            match word_eol.get(1) {
                Option::Some(&s) if s == "true" => {
                    ph.print("Set PrintOldTopic to true");
                    *enabled.lock().unwrap() = true;
                }
                Option::Some(&s) if s == "false" => {
                    ph.print("Set PrintOldTopic to false");
                    *enabled.lock().unwrap() = false;
                }
                Option::Some(_) => {
                    ph.print("Usage: /PrintOldTopic [true|false]");
                }
                Option::None => {
                    ph.print(&format!("PrintOldTopic: {}", *enabled.lock().unwrap()));
                }
            }
            hexchat_plugin::EAT_ALL
        }, hexchat_plugin::PRI_NORM, Some("Usage: /PrintOldTopic [true|false]")));
        true // tells hexchat we have successfully initialized
    }
}

hexchat_plugin!(MyPlug);
```
