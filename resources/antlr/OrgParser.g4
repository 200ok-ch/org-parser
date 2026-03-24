parser grammar OrgParser;

options { tokenVocab=OrgLexer; }

s: lineWithNl* lineAtEof? EOF;

lineWithNl: NEWLINE | line NEWLINE;

lineAtEof: line;

line
  : emptyLine
  | headline
  | todoLine
  | blockBeginLine
  | blockEndLine
  | dynamicBlockBeginLine
  | dynamicBlockEndLine
  | otherKeywordLine
  | footnoteLine
  | commentLine
  | horizontalRule
  | drawerBeginLine
  | drawerEndLine
  | contentLine
  ;

lineEof: line EOF;

drawerBeginLineEof: drawerBeginLine EOF;

drawerEndLineEof: drawerEndLine EOF;

commentLineEof: commentLine EOF;

horizontalRuleEof: horizontalRule EOF;

todoLineEof: todoLine EOF;

blockBeginLineEof: blockBeginLine EOF;

blockEndLineEof: blockEndLine EOF;

dynamicBlockBeginLineEof: dynamicBlockBeginLine EOF;

dynamicBlockEndLineEof: dynamicBlockEndLine EOF;

footnoteLineEof: footnoteLine EOF;

footnoteLinkEof: footnoteLink EOF;

otherKeywordLineEof: otherKeywordLine EOF;

emptyLine: SPACE+;

headline
  : stars (SPACE+ keyword)? (SPACE+ priority)? (SPACE+ commentToken)? SPACE+ title
  ;

stars: STAR+;

keyword: UPPER+;

priority: LBRACK HASH UPPER RBRACK;

commentToken: COMMENT;

title: text;

contentLine: text;

todoLine: HASH PLUS todoKeyword COLON SPACE+ todoState (SPACE+ todoState)* SPACE+ BAR SPACE+ doneState (SPACE+ doneState)*;

todoKeyword: UPPER+;

todoState: UPPER+;

doneState: UPPER+;

blockBeginLine: HASH PLUS blockBeginMarker UNDERSCORE blockName (SPACE blockParameters)? SPACE*;

blockEndLine: HASH PLUS blockEndMarker UNDERSCORE blockName SPACE*;

blockBeginMarker: markerChar+;

blockEndMarker: markerChar+;

blockName: blockNameChar+;

blockNameChar: UPPER | LOWER | TEXT_CHAR;

blockParameters: text;

dynamicBlockBeginLine: HASH PLUS dynamicBeginMarker COLON SPACE+ dynamicBlockName (SPACE dynamicBlockParameters)? SPACE*;

dynamicBlockEndLine: HASH PLUS dynamicEndMarker COLON SPACE*;

dynamicBeginMarker: markerChar+;

dynamicEndMarker: markerChar+;

markerChar: UPPER | LOWER;

dynamicBlockName: dynamicNameChar+;

dynamicNameChar: UPPER | LOWER | TEXT_CHAR;

dynamicBlockParameters: text;

otherKeywordLine: HASH PLUS kwName COLON SPACE kwValue?;

kwName: markerChar+;

kwValue: text;

footnoteLine: LBRACK fnPrefix fnLabel RBRACK SPACE text;

footnoteLink: LBRACK fnPrefix fnLabel RBRACK | LBRACK fnPrefix COLON fnTextInline RBRACK | LBRACK fnPrefix fnLabel COLON fnTextInline RBRACK;

fnPrefix: LOWER LOWER COLON;

fnLabel: fnLabelChar+;

fnLabelChar: UPPER | LOWER | DASH | UNDERSCORE | TEXT_CHAR;

fnTextInline: inlineChar*;

inlineChar: SPACE | UPPER | LOWER | DASH | UNDERSCORE | PLUS | BAR | HASH | COLON | STAR | TEXT_CHAR;

commentLine: SPACE* HASH commentLineRest;

commentLineRest: SPACE text? | ;

horizontalRule: SPACE* DASH DASH DASH DASH DASH DASH*;

drawerBeginLine: COLON drawerName COLON SPACE*;

drawerName: drawerNameChar+;

drawerNameChar: UPPER | LOWER | TEXT_CHAR;

drawerEndLine: END_DRAWER SPACE*;

text: textNormal+;

textNormal: (TEXT_CHAR | SPACE | UPPER | LOWER | DASH | PLUS | BAR | UNDERSCORE | HASH | LBRACK | RBRACK | COLON | STAR)+;
