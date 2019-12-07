# Simple Templating Engine for Free Pascal

Fast engine is used to generate text, html, xml, markup or any text by using tags/variables

## Features

* Pre-assigned variables
* OnExpandTag (global) callback
* Per-tag callbacks
* IF/ELSE blocks
* FOR blocks (dataset iteration)
* Nested blocks
* Separated synthax parsing (prepare) and variable expansion (process) on prepared templates.


## Quickstart

* No installation required, just add steParser and/or steProcessor to **uses** clase where needed in your unit.
```pascal
uses steProcessor;

var
  tplRpoc : TSTEProcessor;
  ResultString : string;

// in this example parser is created internally
tplRpoc := TSTEProcessor.Create;
try
  tplRpoc.SetValue('var', 'your'); 
  ResultString := tplRpocGenerateToString('This is a template text. And here is <?var?> variable');
finally
  tplRpoc.Free;
end;
```
## Templates, keywords


## Minimal example


this doc is still work in progress
