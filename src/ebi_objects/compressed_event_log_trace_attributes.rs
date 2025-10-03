use anyhow::Result;
use flate2::{Compression, bufread::GzDecoder, write::GzEncoder};
use std::io::{BufRead, BufReader, Write};

use crate::{
    EventLogTraceAttributes, HasActivityKey, TranslateActivityKey,
    constants::ebi_object::EbiObject,
    traits::{exportable::Exportable, importable::Importable},
};

pub const FORMAT_SPECIFICATION: &str = "A compressed event log is a gzipped event log file in the IEEE XES format~\\cite{DBLP:journals/cim/AcamporaVSAGV17}.
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.";

pub struct CompressedEventLogTraceAttributes {
    pub log: EventLogTraceAttributes,
}

impl Importable for CompressedEventLogTraceAttributes {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let log = Self::import(reader)?;
        Ok(EbiObject::EventLogTraceAttributes(log.log))
    }

    fn import(reader: &mut dyn BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let dec = GzDecoder::new(reader);
        let mut reader2 = BufReader::new(dec);
        let log = EventLogTraceAttributes::import(&mut reader2)?;
        Ok(Self { log })
    }
}

impl Exportable for CompressedEventLogTraceAttributes {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLogTraceAttributes(log) => Self::export(&Self { log }, f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        let mut writer = GzEncoder::new(f, Compression::best());
        self.log.export(&mut writer)
    }
}

impl HasActivityKey for CompressedEventLogTraceAttributes {
    fn activity_key(&self) -> &crate::ActivityKey {
        self.log.activity_key()
    }

    fn activity_key_mut(&mut self) -> &mut crate::ActivityKey {
        self.log.activity_key_mut()
    }
}

impl TranslateActivityKey for CompressedEventLogTraceAttributes {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut crate::ActivityKey) {
        self.log.translate_using_activity_key(to_activity_key)
    }
}
