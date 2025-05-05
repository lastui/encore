use std::collections::HashMap;  
use std::rc::Rc;
  
pub struct StringInterner {  
    strings: HashMap<String, Rc<str>>,  
}  
  
impl StringInterner {  
    pub fn new() -> Self {  
        StringInterner {  
            strings: HashMap::new(),  
        }  
    }  
  
    pub fn intern(&mut self, s: &str) -> Rc<str> {  
        if let Some(interned) = self.strings.get(s) {  
            interned.clone()  
        } else {  
            // Fix: Use explicit type annotation  
            let rc: Rc<str> = s.into();  
            self.strings.insert(s.to_string(), rc.clone());  
            rc  
        }  
    }  
}