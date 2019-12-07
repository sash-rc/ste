# Simple Templating Engine for Free Pascal

Fast engine to generate text (html, xml, or any markup) with tags/variables in Object/Free Pascal (probably delphi compatible)

## Features

* Pre-assigned variables or dataset field values (no callbacks).
* OnExpandTag (global) callback.
* Per-tag callbacks.
* IF/ELSE blocks.
* FOR blocks (dataset iteration, no callback).
* Nested blocks.
* Custom tag openers/closers.
* Separated synthax parsing (prepare) and variable expansion (process) on prepared templates.
* I/O with strings/streams


## Quickstart

* Download / clone source code.
* No installation required, just include path to *src* to your project (Other Unit Files in your Lazarus project).
* Add steParser and/or steProcessor to **uses** clase as needed in your unit.
* Here is a minimal example
```pascal
uses steProcessor;

var
  tplProc : TSTEProcessor;
  ResultString : string;

// in this example parser is created internally
tplProc := TSTEProcessor.Create;
try
  tplProc.SetValue('var', 'your'); 
  ResultString := tplRpocGenerateToString('This is a template text. And here is <?var?> variable');
finally
  tplProc.Free;
end;
```
## Templates, keywords


## Minimal example


this doc is still work in progress
