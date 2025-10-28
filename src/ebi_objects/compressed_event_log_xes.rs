use crate::{EbiObject, EventLogXes, Exportable, HasActivityKey, Importable, TranslateActivityKey};
use anyhow::Result;
use flate2::{Compression, bufread::GzDecoder, write::GzEncoder};
use std::io::{BufRead, BufReader, Write};

pub struct CompressedEventLogXes {
    pub log: EventLogXes,
}

impl Importable for CompressedEventLogXes {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let log = Self::import(reader)?;
        Ok(EbiObject::EventLogXes(log.log))
    }

    fn import(reader: &mut dyn BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let dec = GzDecoder::new(reader);
        let mut reader2 = BufReader::new(dec);
        let log = EventLogXes::import(&mut reader2)?;
        Ok(Self { log })
    }
}

impl Exportable for CompressedEventLogXes {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLogXes(log) => Self::export(&Self { log }, f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        let mut writer = GzEncoder::new(f, Compression::best());
        self.log.export(&mut writer)
    }
}

impl HasActivityKey for CompressedEventLogXes {
    fn activity_key(&self) -> &crate::ActivityKey {
        self.log.activity_key()
    }

    fn activity_key_mut(&mut self) -> &mut crate::ActivityKey {
        self.log.activity_key_mut()
    }
}

impl TranslateActivityKey for CompressedEventLogXes {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut crate::ActivityKey) {
        self.log.translate_using_activity_key(to_activity_key)
    }
}
