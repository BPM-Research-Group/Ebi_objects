use anyhow::Result;
use flate2::{Compression, bufread::GzDecoder, write::GzEncoder};
use std::io::{BufRead, BufReader, Write};

use crate::{
    EventLog, EventLogTraceAttributes, HasActivityKey, TranslateActivityKey,
    constants::ebi_object::EbiObject,
    traits::{exportable::Exportable, importable::Importable},
};

pub const FORMAT_SPECIFICATION: &str = "A compressed event log is a gzipped event log file in the IEEE XES format~\\cite{DBLP:journals/cim/AcamporaVSAGV17}.
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.";

pub enum CompressedEventLog {
    EventLog(EventLog),
    EventLogTraceAttributes(EventLogTraceAttributes),
}

impl CompressedEventLog {
    pub fn import_as_object_event_log(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dec = GzDecoder::new(reader);
        let mut reader2 = BufReader::new(dec);
        let log = EventLog::import(&mut reader2)?;
        Ok(EbiObject::EventLog(log))
    }
}

impl Importable for CompressedEventLog {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        if let Self::EventLogTraceAttributes(log) = Self::import(reader)? {
            Ok(EbiObject::EventLogTraceAttributes(log))
        } else {
            unreachable!()
        }
    }

    fn import(reader: &mut dyn BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let dec = GzDecoder::new(reader);
        let mut reader2 = BufReader::new(dec);
        let log = EventLogTraceAttributes::import(&mut reader2)?;
        Ok(Self::EventLogTraceAttributes(log))
    }
}

impl Exportable for CompressedEventLog {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLog(log) => Self::export(&Self::EventLog(log), f),
            EbiObject::EventLogTraceAttributes(log) => {
                Self::export(&Self::EventLogTraceAttributes(log), f)
            }
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        let mut writer = GzEncoder::new(f, Compression::best());
        match self {
            CompressedEventLog::EventLog(event_log) => event_log.export(&mut writer),
            CompressedEventLog::EventLogTraceAttributes(event_log_trace_attributes) => {
                event_log_trace_attributes.export(&mut writer)
            }
        }
    }
}

impl HasActivityKey for CompressedEventLog {
    fn activity_key(&self) -> &crate::ActivityKey {
        match self {
            CompressedEventLog::EventLog(event_log) => event_log.activity_key(),
            CompressedEventLog::EventLogTraceAttributes(event_log_trace_attributes) => {
                event_log_trace_attributes.activity_key()
            }
        }
    }

    fn activity_key_mut(&mut self) -> &mut crate::ActivityKey {
        match self {
            CompressedEventLog::EventLog(event_log) => event_log.activity_key_mut(),
            CompressedEventLog::EventLogTraceAttributes(event_log_trace_attributes) => {
                event_log_trace_attributes.activity_key_mut()
            }
        }
    }
}

impl TranslateActivityKey for CompressedEventLog {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut crate::ActivityKey) {
        match self {
            CompressedEventLog::EventLog(event_log) => {
                event_log.translate_using_activity_key(to_activity_key)
            }
            CompressedEventLog::EventLogTraceAttributes(event_log_trace_attributes) => {
                event_log_trace_attributes.translate_using_activity_key(to_activity_key)
            }
        }
    }
}
