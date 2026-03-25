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

nodePropertyLineEof: nodePropertyLine EOF;

propertyDrawerEof: propertyDrawer EOF;

fixedWidthLineEof: fixedWidthLine EOF;

fixedWidthAreaEof: fixedWidthArea EOF;

linkExtOtherEof: linkExtOther EOF;

linkExtIdEof: linkExtId EOF;

linkExtFileEof: linkExtFile EOF;

textLinkEof: textLink EOF;

tsTimeEof: tsTime EOF;

timestampInactiveRangeEof: timestampInactiveRange EOF;

timestampEof: timestamp EOF;

clockEof: clock EOF;

planningEof: planning EOF;

textEntityEof: textEntity EOF;

textTargetEof: textTarget EOF;

textSubEof: textSub EOF;

textMacroEof: textMacro EOF;

tagsEof: tags EOF;

diarySexpEof: diarySexp EOF;

affiliatedKeywordLineEof: affiliatedKeywordLine EOF;

listItemLineEof: listItemLine EOF;

tableEof: table EOF;

textStyledEof: textStyled EOF;

linkFormatEof: linkFormat EOF;

eolEof: eol EOF;

wordEof: word EOF;

horizontalSpaceEof: horizontalSpace EOF;

textSupEof: textSup EOF;

textRadioTargetEof: textRadioTarget EOF;

noparseBlockEof: noparseBlock EOF;

textEof: text EOF;

emptyLine: SPACE+;

eol: NEWLINE?;

horizontalSpace: SPACE+;

word: wordChar+;

wordChar: ~(NEWLINE | SPACE);

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

blockNameChar: ~(NEWLINE | SPACE);

blockParameters: text;

noparseBlock: noparseBlockBeginLine NEWLINE noparseBlockContent noparseBlockEndLine;

noparseBlockBeginLine: SPACE* HASH PLUS noparseBeginMarker UNDERSCORE noparseBlockName (SPACE noparseBlockParameters)? SPACE*;

noparseBlockEndLine: SPACE* HASH PLUS noparseEndMarker UNDERSCORE noparseEndName SPACE*;

noparseBeginMarker: markerChar+;

noparseEndMarker: markerChar+;

noparseBlockName: blockName;

noparseEndName: blockName;

noparseBlockParameters: text;

noparseBlockContent: (.)*?;

dynamicBlockBeginLine: DYNBLOCK_BEGIN_PREFIX SPACE+ dynamicBlockName (SPACE dynamicBlockParameters)? SPACE*;

dynamicBlockEndLine: DYNBLOCK_END_PREFIX SPACE*;

dynamicBeginMarker: DYNBLOCK_BEGIN_PREFIX;

dynamicEndMarker: DYNBLOCK_END_PREFIX;

markerChar: UPPER | LOWER;

dynamicBlockName: dynamicNameChar+;

dynamicNameChar: ~(NEWLINE | SPACE);

dynamicBlockParameters: text;

otherKeywordLine: HASH PLUS kwName COLON SPACE kwValue?;

kwName: markerChar+;

kwValue: text;

footnoteLine: LBRACK fnPrefix fnLabel RBRACK SPACE text;

footnoteLink: LBRACK fnPrefix fnLabel RBRACK | LBRACK fnPrefix COLON fnTextInline RBRACK | LBRACK fnPrefix fnLabel COLON fnTextInline RBRACK;

fnPrefix: LOWER LOWER COLON;

fnLabel: fnLabelChar+;

fnLabelChar: UPPER | LOWER | DIGIT | DASH | UNDERSCORE | DOT;

fnTextInline: inlineChar*;

inlineChar: PLAIN_URL | SPACE | UPPER | LOWER | DIGIT | DASH | UNDERSCORE | PLUS | BAR | HASH | COLON | STAR | LT | GT | LPAREN | RPAREN | LBRACE | RBRACE | BACKSLASH | SLASH | EQUALS | TILDE | DOT | COMMA | PERCENT | AT | QUESTION | EXCL | CARET | TEXT_CHAR;

commentLine: SPACE* HASH commentLineRest;

commentLineRest: SPACE text? | ;

horizontalRule: SPACE* DASH DASH DASH DASH DASH DASH*;

drawerBeginLine: COLON drawerName COLON SPACE*;

drawerName: drawerNameChar+;

drawerNameChar: ~(NEWLINE | COLON);

drawerEndLine: END_DRAWER SPACE*;

text: textSegment* textLinebreak | textSegment+;

textSegment
  : timestamp
  | linkFormat
  | footnoteLink
  | textRadioTarget
  | textTarget
  | textLink
  | textStyled
  | textEntity
  | textSub
  | textSup
  | textMacro
  | textPlain
  | textFallbackChar
  ;

textLinebreak: BACKSLASH BACKSLASH textLinebreakAfter;

textLinebreakAfter: SPACE*;

textPlain: textPlainChar+;

textPlainChar
  : TEXT_CHAR
  | SPACE
  | COMMENT
  | DYNBLOCK_BEGIN_PREFIX
  | DYNBLOCK_END_PREFIX
  | UPPER
  | LOWER
  | DIGIT
  | RBRACK
  | HASH
  | COLON
  | DASH
  | BAR
  | GT
  | LPAREN
  | RPAREN
  | RBRACE
  | DOT
  | COMMA
  | PERCENT
  | AT
  | QUESTION
  | EXCL
  ;

textFallbackChar
  : STAR
  | LBRACK
  | PLUS
  | UNDERSCORE
  | LT
  | LBRACE
  | BACKSLASH
  | SLASH
  | EQUALS
  | TILDE
  | CARET
  ;

textNormal: sameLineChar+;

sameLineChar
  : TEXT_CHAR
  | PLAIN_URL
  | SPACE
  | COMMENT
  | DYNBLOCK_BEGIN_PREFIX
  | DYNBLOCK_END_PREFIX
  | UPPER
  | LOWER
  | DIGIT
  | STAR
  | LBRACK
  | RBRACK
  | HASH
  | COLON
  | DASH
  | PLUS
  | BAR
  | UNDERSCORE
  | LT
  | GT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | BACKSLASH
  | SLASH
  | EQUALS
  | TILDE
  | DOT
  | COMMA
  | PERCENT
  | AT
  | QUESTION
  | EXCL
  | CARET
  ;

sameLineCharNoBar
  : TEXT_CHAR
  | PLAIN_URL
  | SPACE
  | COMMENT
  | UPPER
  | LOWER
  | DIGIT
  | STAR
  | LBRACK
  | RBRACK
  | HASH
  | COLON
  | DASH
  | PLUS
  | UNDERSCORE
  | LT
  | GT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | BACKSLASH
  | SLASH
  | EQUALS
  | TILDE
  | DOT
  | COMMA
  | PERCENT
  | AT
  | QUESTION
  | EXCL
  | CARET
  ;

sameLineCharNoRbrack
  : TEXT_CHAR
  | SPACE
  | COMMENT
  | UPPER
  | LOWER
  | DIGIT
  | STAR
  | LBRACK
  | HASH
  | COLON
  | DASH
  | PLUS
  | BAR
  | UNDERSCORE
  | LT
  | GT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | BACKSLASH
  | SLASH
  | EQUALS
  | TILDE
  | DOT
  | COMMA
  | PERCENT
  | AT
  | QUESTION
  | EXCL
  | CARET
  ;

tags: COLON tagName (COLON tagName)* COLON;

tagName: tagNameChar+;

tagNameChar: UPPER | LOWER | DIGIT | UNDERSCORE | AT | HASH | PERCENT;

diarySexp: PERCENT PERCENT diarySexpDirectBody;

diarySexpDirectBody: sameLineChar+;

affiliatedKeywordLine: SPACE* HASH PLUS affiliatedKeywordName affiliatedKeywordOption? COLON SPACE+ affiliatedKeywordValue;

affiliatedKeywordName: affiliatedKeywordNameChar+;

affiliatedKeywordNameChar: UPPER | LOWER | DIGIT | DASH | UNDERSCORE;

affiliatedKeywordOption: LBRACK affiliatedKeywordOptionText RBRACK;

affiliatedKeywordOptionText: sameLineCharNoRbrack+;

affiliatedKeywordValue: sameLineCharNoRbrack+;

nodePropertyLine: COLON nodePropertyName PLUS? COLON (SPACE nodePropertyValue)?;

nodePropertyName: nodePropertyNameChar+;

nodePropertyNameChar: ~(NEWLINE | SPACE | COLON | PLUS);

nodePropertyValue: sameLineChar+;

propertyDrawer: drawerBeginLine NEWLINE (nodePropertyLine NEWLINE)* drawerEndLine;

fixedWidthLine: SPACE* COLON (SPACE fixedWidthValue?)?;

fixedWidthValue: sameLineChar+;

fixedWidthArea: fixedWidthLine (NEWLINE fixedWidthLine)* NEWLINE?;

linkUrlScheme: (UPPER | LOWER) (UPPER | LOWER | DIGIT | PLUS | DASH | DOT)*;

linkUrlRest: sameLineChar+;

linkExtOther: PLAIN_URL | linkUrlScheme COLON linkUrlRest?;

linkExtId: LBRACK LBRACK linkIdPrefix COLON linkIdValue RBRACK RBRACK;

linkIdPrefix: LOWER LOWER;

linkIdValue: (UPPER | LOWER | DIGIT) (UPPER | LOWER | DIGIT | DASH)+;

linkExtFile: linkFileScheme? linkFilePath (COLON COLON linkFileLocation)?;

linkFileScheme: (UPPER | LOWER) (UPPER | LOWER) (UPPER | LOWER) (UPPER | LOWER) COLON;

linkFilePath: linkFilePathChar+?;

linkFilePathChar: sameLineChar;

linkFileLocation: linkFileLocationLine | linkFileLocationHeadline | linkFileLocationCustomId | linkFileLocationString;

linkFileLocationLine: DIGIT+;

linkFileLocationHeadline: STAR sameLineChar+;

linkFileLocationCustomId: HASH sameLineChar+;

linkFileLocationString: sameLineChar+;

textLink: textLinkAngle | textLinkPlain;

textLinkAngle: LT linkExtOther GT;

textLinkPlain: linkExtOther;

tsDate: DIGIT DIGIT DIGIT DIGIT DASH DIGIT DIGIT DASH DIGIT DIGIT;

tsDay: tsDayChar+;

tsDayChar: ~(NEWLINE | SPACE | LT | GT | LBRACK | RBRACK | DIGIT);

tsTime: DIGIT DIGIT? COLON DIGIT DIGIT (COLON DIGIT DIGIT)? ((UPPER | LOWER) (UPPER | LOWER))?;

tsModValue: DIGIT+;

tsModUnit: UPPER | LOWER;

tsModAtLeast: SLASH tsModValue tsModUnit;

tsRepeaterType: PLUS PLUS | PLUS | DOT PLUS;

tsWarningType: DASH DASH | DASH;

tsRepeater: tsRepeaterType tsModValue tsModUnit tsModAtLeast?;

tsWarning: tsWarningType tsModValue tsModUnit tsModAtLeast?;

tsModifier: tsRepeater | tsWarning;

tsModifiers: tsModifier (SPACE+ tsModifier)*;

tsInner: tsDate (SPACE+ tsDay)? (SPACE+ tsTime)? (SPACE+ tsModifiers)?;

tsInnerWTime: tsDate (SPACE+ tsDay)? SPACE+ tsTime;

tsInnerSpan: tsDate (SPACE+ tsDay)? SPACE+ tsTime DASH tsTime (SPACE+ tsModifiers)?;

diarySexpBody: diarySexpChar+;

diarySexpChar: ~(NEWLINE | GT);

timestampInactiveRange
  : LBRACK tsInnerSpan RBRACK
  | LBRACK tsInnerWTime RBRACK DASH DASH LBRACK tsInnerWTime RBRACK
  ;

timestamp
  : LT PERCENT PERCENT diarySexpBody GT
  | LT tsInnerSpan GT
  | LBRACK tsInnerSpan RBRACK
  | LT tsInner GT DASH DASH LT tsInner GT
  | LBRACK tsInner RBRACK DASH DASH LBRACK tsInner RBRACK
  | LT tsInner GT
  | LBRACK tsInner RBRACK
  ;

clock: SPACE* clockKeyword COLON SPACE* timestampInactiveRange SPACE* EQUALS GT SPACE* clockHours COLON clockMinutes SPACE*;

clockKeyword: UPPER+;

clockHours: DIGIT+;

clockMinutes: DIGIT DIGIT;

planning: SPACE* planningInfo (SPACE+ planningInfo)* SPACE*;

planningInfo: planningKeyword COLON SPACE* planningTimestamp;

planningKeyword: UPPER+;

planningTimestamp: LBRACK tsInner RBRACK | LT tsInner GT;

textEntity: BACKSLASH entityName (LBRACE RBRACE)?;

entityName: (UPPER | LOWER)+;

textTarget: LT LT textTargetName GT GT;

textTargetName: textTargetNameEdge | textTargetNameEdge (textTargetNameMiddle | SPACE)* textTargetNameEdge;

textTargetNameEdge: ~(NEWLINE | SPACE | LT | GT);

textTargetNameMiddle: ~(NEWLINE | LT | GT);

textSub: UNDERSCORE LBRACE textSubCurlyBody RBRACE | UNDERSCORE textSubWord;

textSubWord: textSubWordChar+;

textSubWordChar: ~(NEWLINE | SPACE);

textSubCurlyBody: textSubCurlyChar+;

textSubCurlyChar: ~(NEWLINE | LBRACE | RBRACE);

textMacro: LBRACE LBRACE LBRACE macroName LPAREN macroArgs? RPAREN RBRACE RBRACE RBRACE;

macroName: (UPPER | LOWER | DIGIT | UNDERSCORE)+;

macroArgs: macroArg (COMMA macroArg)*;

macroArg: macroArgPart+;

macroArgPart: BACKSLASH COMMA | LPAREN macroArgPart* RPAREN | macroArgChar;

macroArgChar: ~(NEWLINE | COMMA | RPAREN);

listItemLine: listItemIndent listItemMarker listItemCheckbox? listItemTagSpec? listItemContents;

listItemIndent: SPACE*;

listItemMarker: listItemBullet SPACE | listItemCounter listItemCounterSuffix SPACE;

listItemBullet: STAR | PLUS | DASH;

listItemCounter: UPPER | LOWER | DIGIT;

listItemCounterSuffix: DOT | RPAREN;

listItemCheckbox: LBRACK listItemCheckboxState RBRACK SPACE;

listItemCheckboxState: SPACE | DASH | UPPER;

listItemTagSpec: listItemTagText SPACE COLON COLON SPACE;

listItemTagText: sameLineChar+;

listItemContents: text;

table: tableOrg | tableTableel;

tableTableel: tableTableelLine (NEWLINE tableTableelLine)* NEWLINE?;

tableTableelLine: SPACE* (tableTableelSepLine | tableTableelTextLine);

tableTableelSepLine: PLUS tableTableelSepChar+ PLUS;

tableTableelSepChar: PLUS | DASH;

tableTableelTextLine: BAR tableTableelTextChar* BAR;

tableTableelTextChar: sameLineChar;

tableOrg: tableOrgLine (NEWLINE tableOrgLine)* NEWLINE?;

tableOrgLine: SPACE* (tableFormulaLine | tableOrgSepLine | tableOrgDataRow);

tableFormulaLine: HASH PLUS tableFormulaKeyword COLON SPACE tableFormulaTextOpt;

tableFormulaKeyword: UPPER+;

tableFormulaTextOpt: sameLineChar*;

tableOrgSepLine: BAR tableOrgSepChar+ BAR;

tableOrgSepChar: PLUS | DASH;

tableOrgDataRow: BAR tableOrgCell (BAR tableOrgCell)* BAR;

tableOrgCell: sameLineCharNoBar*;

textStyled
  : textStyledBold
  | textStyledItalic
  | textStyledUnderlined
  | textStyledVerbatim
  | textStyledCode
  | textStyledStrike
  ;

textStyledBold: STAR textStyledBody STAR;

textStyledItalic: SLASH textStyledBody SLASH;

textStyledUnderlined: UNDERSCORE textStyledBody UNDERSCORE;

textStyledVerbatim: EQUALS textStyledBody EQUALS;

textStyledCode: TILDE textStyledBody TILDE;

textStyledStrike: PLUS textStyledBody PLUS;

textStyledBody: sameLineChar+?;

linkFormat: LBRACK LBRACK linkTarget RBRACK (RBRACK | LBRACK linkDescriptionRaw RBRACK RBRACK);

linkTarget: linkTargetId | linkTargetExtOther | linkTargetIntCustomId | linkTargetIntHeadline | linkTargetIntString;

linkTargetId: LOWER LOWER COLON linkIdValue;

linkTargetExtOther: PLAIN_URL | linkUrlScheme COLON linkTargetRest?;

linkTargetIntCustomId: HASH linkTargetIntBody;

linkTargetIntHeadline: STAR linkTargetIntBody;

linkTargetIntString: linkTargetIntBody;

linkTargetIntBody: linkTargetIntText+;

linkTargetRest: linkTargetChunk+;

linkTargetIntText: linkTargetChunk;

linkTargetChunk: BACKSLASH linkTargetEscapedChar | linkTargetPlainChar;

linkTargetEscapedChar: ~(NEWLINE);

linkTargetPlainChar: ~(NEWLINE | LBRACK | RBRACK | BACKSLASH);

linkDescriptionRaw: sameLineChar*;

textSup: CARET LBRACE textSubCurlyBody RBRACE | CARET textSupWord;

textSupWord: textSupWordChar+;

textSupWordChar: ~(NEWLINE | SPACE | UNDERSCORE);

textRadioTarget: LT LT LT textRadioTargetBody GT GT GT;

textRadioTargetBody: textRadioTargetBodyChar+;

textRadioTargetBodyChar: ~(NEWLINE | GT);
