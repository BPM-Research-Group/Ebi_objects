use crate::BusinessProcessModelAndNotation;
use anyhow::{Result, anyhow};

impl BusinessProcessModelAndNotation {
    pub fn is_structurally_correct(&self) -> Result<()> {
        //check messages
        for message_flow in &self.message_flows {
            //each message must connect different pools
            if message_flow.source_pool_index == message_flow.target_pool_index {
                return Err(anyhow!(
                    "message flow with id `{}` is intra-pool",
                    message_flow.id
                ));
            }

            //a message may only be sent by an object that can send
            if let Some(obj) = self.find_object_with_index(message_flow.source_element_index) {
                if !obj.can_throw_message() {
                    return Err(anyhow!(
                        "message flow `{}` comes from object `{}` that cannot throw a message",
                        message_flow.id,
                        obj.id()
                    ));
                }
            } else {
                return Err(anyhow!(
                    "message flow `{}` has a source that cannot be found",
                    message_flow.id
                ));
            }

            //a message may only be received by an object that can catch
            if let Some(obj) = self.find_object_with_index(message_flow.target_element_index) {
                if !obj.can_catch_message() {
                    return Err(anyhow!(
                        "message flow `{}` goes to object `{}` that cannot catch a message",
                        message_flow.id,
                        obj.id()
                    ));
                }
            } else {
                return Err(anyhow!(
                    "message flow `{}` has a target that cannot be found",
                    message_flow.id
                ));
            }
        }

        Ok(())
    }
}
