use super::{ModuleInterface, ModuleInterfaceCodec};

pub struct JsonModuleInterfaceCodec;

impl ModuleInterfaceCodec for JsonModuleInterfaceCodec {
    type Error = serde_json::Error;

    fn encode(interface: &ModuleInterface) -> Result<Vec<u8>, Self::Error> {
        serde_json::to_vec_pretty(interface)
    }

    fn decode(bytes: &[u8]) -> Result<ModuleInterface, Self::Error> {
        serde_json::from_slice(bytes)
    }
}
