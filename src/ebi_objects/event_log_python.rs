use crate::{
    ActivityKey, EbiObject, EventLogXes, Exportable, HasActivityKey, Importable, Infoable,
    NumberOfTraces, TranslateActivityKey,
    traits::importable::{ImporterParameter, ImporterParameterValues},
};
use anyhow::{Result, anyhow};
use std::{
    fmt::Display,
    io::{BufRead, Write},
};

#[derive(Clone)]
/// A Python log is an XES log, but without trace attributes.
/// There are no importers, as Python logs can only come from Python.
pub struct EventLogPython {
    pub log: EventLogXes,
}

impl Importable for EventLogPython {
    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "A Pm4py log can be imported by Ebi from Python. Ensure to set \\texttt{return\\_legacy\\_log\\_object=True} at importing.";

    fn import_as_object(
        _reader: &mut dyn BufRead,
        _parameter_values: &ImporterParameterValues,
    ) -> anyhow::Result<EbiObject> {
        Err(anyhow!("A Python log can only be imported from Python."))
    }

    fn import(
        _reader: &mut dyn BufRead,
        _parameter_values: &ImporterParameterValues,
    ) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        Err(anyhow!("A Python log can only be imported from Python."))
    }
}

impl Display for EventLogPython {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.log.fmt(f)
    }
}

impl Exportable for EventLogPython {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        EventLogXes::export_from_object(object, f)
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        EventLogXes::export(&self.log, f)
    }
}

impl Infoable for EventLogPython {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        self.log.info(f)
    }
}

impl NumberOfTraces for EventLogPython {
    fn number_of_traces(&self) -> usize {
        self.log.number_of_traces()
    }

    fn number_of_events(&self) -> usize {
        self.log.number_of_events()
    }
}

impl HasActivityKey for EventLogPython {
    fn activity_key(&self) -> &ActivityKey {
        self.log.activity_key()
    }

    fn activity_key_mut(&mut self) -> &mut ActivityKey {
        self.log.activity_key_mut()
    }
}

impl TranslateActivityKey for EventLogPython {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        self.log.translate_using_activity_key(to_activity_key);
    }
}
