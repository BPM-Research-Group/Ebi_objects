use crate::{
    BusinessProcessModelAndNotation,
    bpmn::{element::BPMNElementTrait, objects_objectable::BPMNObject},
};
use anyhow::{Context, Result, anyhow};

impl BusinessProcessModelAndNotation {
    pub fn is_structurally_correct(&self) -> Result<()> {
        //check elements
        for element in self.all_elements_ref() {
            element
                .verify_structural_correctness(self)
                .with_context(|| anyhow!("element `{}`", element.id()))?;
        }

        //check messages
        for message_flow in &self.message_flows {
            //each message must connect different pools
            if message_flow.source_pool_index == message_flow.target_pool_index {
                return Err(anyhow!(
                    "message flow with id `{}` is intra-pool",
                    message_flow.id
                ));
            }
        }

        Ok(())
    }
}
