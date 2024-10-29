module Types
  ( AcceptSuggestion
  , AccessibilitySupport
  , Editor
  , EditorConstructionOptions
  , EditorFindOptions
  , EditorMinimapOptions
  , EditorOptionsMixin
  , EditorScrollbarOptions
  , FontWeight
  , LineNumberFunction
  , LineNumbers
  , MONACO
  , MultiCursorModifier
  , RenderHighLight
  , RenderWhiteSpace
  , ScrollbarVisibility
  , ShowFoldingControls
  , ShowSlider
  , SnippetSuggestion
  , WordWrapState
  , acceptSuggestionOff
  , acceptSuggestionOn
  , acceptSuggestionSmart
  , accessibilitySupportAuto
  , accessibilitySupportOff
  , accessibilitySupportOn
  , alt
  , ctrlCmd
  , defaultConstuctorOptions
  , defaultEditorScrollbarOptions
  , defaultMinimapOptions
  , fontWeight100
  , fontWeight200
  , fontWeight300
  , fontWeight400
  , fontWeight500
  , fontWeight600
  , fontWeight700
  , fontWeight800
  , fontWeight900
  , fontWeightBold
  , fontWeightBolder
  , fontWeightInherit
  , fontWeightInitial
  , fontWeightLighter
  , fontWeightNormal
  , lineNumberFunction
  , lineNumbersOff
  , lineNumbersOn
  , lineNumbersRelative
  , renderHighLightAll
  , renderHighLightGutter
  , renderHighLightLine
  , renderHighLightNone
  , renderWhiteSpaceAll
  , renderWhiteSpaceBoundary
  , renderWhiteSpaceNone
  , scrollbarVisibilityAuto
  , scrollbarVisibilityHidden
  , scrollbarVisibilityVisible
  , showFoldingControlsAlways
  , showFoldingControlsMouseOver
  , showSliderAlways
  , showSliderMouseOver
  , snippetSuggestionBottom
  , snippetSuggestionInline
  , snippetSuggestionNone
  , snippetSuggestionTop
  , wordwrapBounded
  , wordwrapColumn
  , wordwrapOff
  , wordwrapOn
  )
  where 

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Record.Builder (build, merge)

foreign import data Editor ∷ Type

foreign import data MONACO :: Type


newtype FontWeight = FontWeight String

fontWeightNormal :: FontWeight
fontWeightNormal = FontWeight "normal"

fontWeightBold :: FontWeight
fontWeightBold = FontWeight "bold"

fontWeightBolder :: FontWeight
fontWeightBolder = FontWeight "Bolder"

fontWeightLighter :: FontWeight
fontWeightLighter = FontWeight "lighter"

fontWeightInitial :: FontWeight
fontWeightInitial = FontWeight "initial"

fontWeightInherit :: FontWeight
fontWeightInherit = FontWeight "inherit"

fontWeight100 :: FontWeight
fontWeight100 = FontWeight "100"

fontWeight200 :: FontWeight
fontWeight200 = FontWeight "200"

fontWeight300 :: FontWeight
fontWeight300 = FontWeight "300"

fontWeight400 :: FontWeight
fontWeight400 = FontWeight "400"

fontWeight500 :: FontWeight
fontWeight500 = FontWeight "500"

fontWeight600 :: FontWeight
fontWeight600 = FontWeight "600"

fontWeight700 :: FontWeight
fontWeight700 = FontWeight "700"

fontWeight800 :: FontWeight
fontWeight800 = FontWeight "800"

fontWeight900 :: FontWeight
fontWeight900 = FontWeight "900"

newtype WordWrapState = WordWrapState String
wordwrapOff :: WordWrapState
wordwrapOff = WordWrapState "off"

wordwrapOn :: WordWrapState
wordwrapOn = WordWrapState "on"

wordwrapColumn :: WordWrapState
wordwrapColumn = WordWrapState "wordWrapColumn"

wordwrapBounded :: WordWrapState
wordwrapBounded = WordWrapState "bounded"


newtype MultiCursorModifier = MultiCursorModifier String
ctrlCmd :: MultiCursorModifier 
ctrlCmd = MultiCursorModifier "ctrlCmd"

alt :: MultiCursorModifier 
alt = MultiCursorModifier "alt"

newtype AccessibilitySupport = AccessibilitySupport String
accessibilitySupportOn :: AccessibilitySupport 
accessibilitySupportOn = AccessibilitySupport "on"

accessibilitySupportOff :: AccessibilitySupport 
accessibilitySupportOff = AccessibilitySupport "off"

accessibilitySupportAuto :: AccessibilitySupport 
accessibilitySupportAuto = AccessibilitySupport "auto"

newtype AcceptSuggestion = AcceptSuggestion String

acceptSuggestionOn :: AcceptSuggestion
acceptSuggestionOn = AcceptSuggestion "on"

acceptSuggestionOff :: AcceptSuggestion
acceptSuggestionOff = AcceptSuggestion "off"

acceptSuggestionSmart :: AcceptSuggestion
acceptSuggestionSmart = AcceptSuggestion "smart"

newtype SnippetSuggestion = SnippetSuggestion String

snippetSuggestionTop :: SnippetSuggestion
snippetSuggestionTop = SnippetSuggestion "top"

snippetSuggestionBottom :: SnippetSuggestion
snippetSuggestionBottom = SnippetSuggestion "bottom"

snippetSuggestionInline :: SnippetSuggestion
snippetSuggestionInline = SnippetSuggestion "inline"

snippetSuggestionNone :: SnippetSuggestion
snippetSuggestionNone = SnippetSuggestion "none"

newtype ShowFoldingControls = ShowFoldingControls String

showFoldingControlsAlways :: ShowFoldingControls 
showFoldingControlsAlways = ShowFoldingControls "always"

showFoldingControlsMouseOver :: ShowFoldingControls 
showFoldingControlsMouseOver = ShowFoldingControls "mouseover"

newtype RenderWhiteSpace = RenderWhiteSpace String 

renderWhiteSpaceNone :: RenderWhiteSpace
renderWhiteSpaceNone = RenderWhiteSpace "none"

renderWhiteSpaceBoundary :: RenderWhiteSpace
renderWhiteSpaceBoundary = RenderWhiteSpace "boundary"

renderWhiteSpaceAll :: RenderWhiteSpace
renderWhiteSpaceAll = RenderWhiteSpace "all"

newtype RenderHighLight = RenderHighLight String

renderHighLightNone :: RenderHighLight
renderHighLightNone = RenderHighLight "none"

renderHighLightGutter :: RenderHighLight
renderHighLightGutter = RenderHighLight "gutter"

renderHighLightLine :: RenderHighLight
renderHighLightLine = RenderHighLight "line"

renderHighLightAll :: RenderHighLight
renderHighLightAll = RenderHighLight "all"

type LineNumberFunction = Number -> String

newtype LineNumbers = LineNumbers (Either String LineNumberFunction)
lineNumbersOn :: LineNumbers
lineNumbersOn = LineNumbers (Left "on")

lineNumbersOff :: LineNumbers
lineNumbersOff = LineNumbers (Left "off")

lineNumbersRelative:: LineNumbers
lineNumbersRelative = LineNumbers (Left "relative")

lineNumberFunction :: LineNumberFunction -> LineNumbers
lineNumberFunction f = LineNumbers (Right f)
 

type EditorOptionsMixin a = {
    -- | The aria label for the editor's textarea (when it is focused).
    ariaLabel :: Maybe String,

    -- | Render vertical lines at the specified columns.
    -- | Defaults to empty array.
    rulers :: Maybe (Array Number),

    -- | A String containing the word separators used when doing word navigation.
    -- | Defaults to `~!@#$%^&*()-=+[{]}\\|;:\'",.<>/?
    wordSeparators :: Maybe String,

    -- | Enable Linux primary clipboard.
    -- | Defaults to true.
    selectionClipboard :: Maybe Boolean,

    -- | Control the rendering of line Numbers.
    -- | If it is a function, it will be invoked when rendering a line Number and the return value will be rendered.
    -- | Otherwise, if it is a truey, line Numbers will be rendered normally (equivalent of using an identity function).
    -- | Otherwise, line Numbers will not be rendered.
    -- | Defaults to true.
    lineNumbers :: Maybe LineNumbers,

    -- | Should the corresponding line be selected when clicking on the line Number?
    -- | Defaults to true.
    selectOnLineNumbers :: Maybe Boolean,

    -- | Control the width of line Numbers, by reserving horizontal space for rendering at least an amount of digits.
    -- | Defaults to 5.
    lineNumbersMinChars :: Maybe Number,

    -- | Enable the rendering of the glyph margin.
    -- | Defaults to true in vscode and to false in monaco-editor.
    glyphMargin :: Maybe Boolean,

    -- | The width reserved for line decorations (in px).
    -- | Line decorations are placed between line Numbers and the editor content.
    -- | You can pass in a String in the format floating point followed by "ch". e.g. 1.3ch.
    -- | Defaults to 10.
    lineDecorationsWidth :: Maybe (Either Number String),

    -- | When revealing the cursor, a virtual padding (px) is added to the cursor, turning it into a rectangle.
    -- | This virtual padding ensures that the cursor gets revealed before hitting the edge of the viewport.
    -- | Defaults to 30 (px).
    revealHorizontalRightPadding :: Maybe Number,

    -- | Render the editor selection with rounded borders.
    -- | Defaults to true.
    roundedSelection :: Maybe Boolean,

    -- | Class name to be added to the editor.
    extraEditorClassName :: Maybe String,

    -- | Should the editor be read only.
    -- | Defaults to false.
    readOnly :: Maybe Boolean,

    -- | Control the behavior and rendering of the scrollbars.
    scrollbar :: Maybe EditorScrollbarOptions,

    -- | Control the behavior and rendering of the minimap.
    minimap :: Maybe EditorMinimapOptions,

    -- | Control the behavior of the find widget.
    find :: Maybe EditorFindOptions,

    -- | Display overflow widgets as `fixed`.
    -- | Defaults to `false`.
    fixedOverflowWidgets :: Maybe Boolean,

    -- | The Number of vertical lanes the overview ruler should render.
    -- | Defaults to 2.
    overviewRulerLanes :: Maybe Number,

    -- | Controls if a border should be drawn around the overview ruler.
    -- | Defaults to `true`.
    overviewRulerBorder :: Maybe Boolean,

    -- | Control the cursor animation style, possible values are 'blink', 'smooth', 'phase', 'expand' and 'solid'.
    -- | Defaults to 'blink'.
    cursorBlinking :: Maybe String,

    -- | Zoom the font in the editor when using the mouse wheel in combination with holding Ctrl.
    -- | Defaults to false.
    mouseWheelZoom :: Maybe Boolean,

    -- | Control the cursor style, either 'block' or 'line'.
    -- | Defaults to 'line'.
    cursorStyle :: Maybe String,

    -- | Enable font ligatures.
    -- | Defaults to false.
    fontLigatures :: Maybe Boolean,

    -- | Disable the use of `will-change` for the editor margin and lines layers.
    -- | The usage of `will-change` acts as a hint for browsers to create an extra layer.
    -- | Defaults to false.
    disableLayerHinting :: Maybe Boolean,

    -- | Disable the optimizations for monospace fonts.
    -- | Defaults to false.
    disableMonospaceOptimizations :: Maybe Boolean,

    -- | Should the cursor be hidden in the overview ruler.
    -- | Defaults to false.
    hideCursorInOverviewRuler :: Maybe Boolean,

    -- | Enable that scrolling can go one screen size after the last line.
    -- | Defaults to true.
    scrollBeyondLastLine :: Maybe Boolean,

    -- | Enable that the editor will install an interval to check if its container dom node size has changed.
    -- | Enabling this might have a severe performance impact.
    -- | Defaults to false.
    automaticLayout :: Maybe Boolean,

    -- | Control the wrapping of the editor.
    -- | When `wordWrap` = "off", the lines will never wrap.
    -- | When `wordWrap` = "on", the lines will wrap at the viewport width.
    -- | When `wordWrap` = "wordWrapColumn", the lines will wrap at `wordWrapColumn`.
    -- | When `wordWrap` = "bounded", the lines will wrap at min(viewport width, wordWrapColumn).
    -- | Defaults to "off".
    wordWrap :: Maybe WordWrapState,

    -- | Control the wrapping of the editor.
    -- | When `wordWrap` = "off", the lines will never wrap.
    -- | When `wordWrap` = "on", the lines will wrap at the viewport width.
    -- | When `wordWrap` = "wordWrapColumn", the lines will wrap at `wordWrapColumn`.
    -- | When `wordWrap` = "bounded", the lines will wrap at min(viewport width, wordWrapColumn).
    -- | Defaults to 80.
    wordWrapColumn :: Maybe Number,

    -- | Force word wrapping when the text appears to be of a minified/generated file.
    -- | Defaults to true.
    wordWrapMinified :: Maybe Boolean,

    -- | Control indentation of wrapped lines. Can be: 'none', 'same' or 'indent'.
    -- | Defaults to 'same' in vscode and to 'none' in monaco-editor.
    wrappingIndent :: Maybe String,

    -- | Configure word wrapping characters. A break will be introduced before these characters.
    -- | Defaults to '{([+'.
    wordWrapBreakBeforeCharacters :: Maybe String, 

    -- | Configure word wrapping characters. A break will be introduced after these characters.
    -- | Defaults to ' \t})]?|&,;'.
    wordWrapBreakAfterCharacters :: Maybe String,

    -- | Configure word wrapping characters.
    -- | A break will be introduced after these characters only if
    -- | no `wordWrapBreakBeforeCharacters` or `wordWrapBreakAfterCharacters` were found.
    -- | Defaults to '.'.
    wordWrapBreakObtrusiveCharacters :: Maybe String, 
    -- | Performance guard: Stop rendering a line after x characters.
    -- | Defaults to 10000.
    -- | Use -1 to never stop rendering
    stopRenderingLineAfter :: Maybe Number,

    -- | Enable hover.
    -- | Defaults to true.
    hover :: Maybe Boolean,

    -- | Enable detecting links and making them clickable.
    -- | Defaults to true.
    links :: Maybe Boolean,

    -- | Enable custom contextmenu.
    -- | Defaults to true.
    contextmenu :: Maybe Boolean,

    -- | A multiplier to be used on the `deltaX` and `deltaY` of mouse wheel scroll events.
    -- | Defaults to 1.
    mouseWheelScrollSensitivity :: Maybe Number,

    -- | The modifier to be used to add multiple cursors with the mouse.
    -- | Defaults to 'alt'
    multiCursorModifier :: Maybe MultiCursorModifier,

    -- | Configure the editor's accessibility support.
    -- | Defaults to 'auto'. It is best to leave this to 'auto'.
    accessibilitySupport :: Maybe AccessibilitySupport,

    -- | Enable quick suggestions (shadow suggestions)
    -- | Defaults to true.
    quickSuggestions :: Maybe ( Either Boolean {
                    other       :: Boolean,
                    comments    :: Boolean,
                    strings     :: Boolean
                }),

    -- | Quick suggestions show delay (in ms)
    -- | Defaults to 500 (ms)
    quickSuggestionsDelay :: Maybe Number,

    -- | Enables parameter hints
    parameterHints :: Maybe Boolean,

    -- | Render icons in suggestions box.
    -- | Defaults to true.
    iconsInSuggestions :: Maybe Boolean,

    -- | Enable auto closing brackets.
    -- | Defaults to true.
    autoClosingBrackets :: Maybe Boolean,

    -- | Enable auto indentation adjustment.
    -- | Defaults to false.
    autoIndent :: Maybe Boolean,

    -- | Enable format on type.
    -- | Defaults to false.
    formatOnType :: Maybe Boolean,

    -- | Enable format on paste.
    -- | Defaults to false.
    formatOnPaste :: Maybe Boolean,

    -- | Controls if the editor should allow to move selections via drag and drop.
    -- | Defaults to false.
    dragAndDrop :: Maybe Boolean,

    -- | Enable the suggestion box to pop-up on trigger characters.
    -- | Defaults to true.
    suggestOnTriggerCharacters :: Maybe Boolean,

    -- | Accept suggestions on ENTER.
    -- | Defaults to 'on'.
    acceptSuggestionOnEnter :: Maybe AcceptSuggestion,

    -- | Accept suggestions on provider defined characters.
    -- | Defaults to true.
    acceptSuggestionOnCommitCharacter :: Maybe Boolean,

    -- | Enable snippet suggestions. Default to 'true'.
    snippetSuggestions :: Maybe SnippetSuggestion,

    -- | Copying without a selection copies the current line.
    emptySelectionClipboard :: Maybe Boolean,

    -- | Enable word based suggestions. Defaults to 'true'
    wordBasedSuggestions :: Maybe Boolean,

    -- | The font size for the suggest widget.
    -- | Defaults to the editor font size.
    suggestFontSize :: Maybe Number,

    -- | The line height for the suggest widget.
    -- | Defaults to the editor line height.
    suggestLineHeight :: Maybe Number,

    -- | Enable selection highlight.
    -- | Defaults to true.
    selectionHighlight :: Maybe Boolean,

    -- | Enable semantic occurrences highlight.
    -- | Defaults to true.
    occurrencesHighlight :: Maybe Boolean,

    -- | Show code lens
    -- | Defaults to true.
    codeLens :: Maybe Boolean,

    -- | Enable code folding
    -- | Defaults to true in vscode and to false in monaco-editor.
    folding :: Maybe Boolean,

    -- | Controls whether the fold actions in the gutter stay always visible or hide unless the mouse is over the gutter.
    -- | Defaults to 'mouseover'.
    showFoldingControls :: Maybe ShowFoldingControls,

    -- | Enable highlighting of matching brackets.
    -- | Defaults to true.
    matchBrackets :: Maybe Boolean,

    -- | Enable rendering of whitespace.
    -- | Defaults to none.
    renderWhitespace :: Maybe RenderWhiteSpace,

    -- | Enable rendering of control characters.
    -- | Defaults to false.
    renderControlCharacters :: Maybe Boolean,

    -- | Enable rendering of indent guides.
    -- | Defaults to false.
    renderIndentGuides :: Maybe Boolean,

    -- | Enable rendering of current line highlight.
    -- | Defaults to all.
    renderLineHighlight :: Maybe RenderHighLight,

    -- | Inserting and deleting whitespace follows tab stops.
    useTabStops :: Maybe Boolean,

    -- | The font family
    fontFamily :: Maybe String,

    -- | The font weight
    fontWeight :: Maybe FontWeight,

    -- | The font size
    fontSize :: Maybe Number,

    -- | The line height
    lineHeight :: Maybe Number,

    -- | The letter spacing
    letterSpacing :: Maybe Number

    | a
}


type EditorConstructionOptions =  EditorOptionsMixin (
    value :: Maybe String,
    language :: Maybe String,
    theme :: Maybe String,
    accessibilityHelpUrl :: Maybe String
) 

defaultConstuctorOptions :: EditorConstructionOptions
defaultConstuctorOptions = build (merge baseOpts) opts
    where 
        baseOpts = defaultOptions
        opts = {
            value : Nothing,
            language : Nothing,
            theme : Nothing,
            accessibilityHelpUrl: Nothing
        }

defaultOptions :: EditorOptionsMixin ()
defaultOptions = 
    { ariaLabel: Nothing
    , rulers: Nothing
    , wordSeparators: Nothing
    , selectionClipboard: Nothing
    , lineNumbers: Nothing
    , selectOnLineNumbers: Nothing
    , lineNumbersMinChars: Nothing
    , glyphMargin: Nothing
    , lineDecorationsWidth: Nothing
    , revealHorizontalRightPadding: Nothing
    , roundedSelection: Nothing
    , extraEditorClassName: Nothing
    , readOnly: Nothing
    , scrollbar: Nothing
    , minimap : Nothing
    , find : Nothing
    , fixedOverflowWidgets: Nothing
    , overviewRulerLanes: Nothing
    , overviewRulerBorder: Nothing
    , cursorBlinking: Nothing
    , mouseWheelZoom: Nothing
    , cursorStyle: Nothing
    , fontLigatures: Nothing
    , disableLayerHinting: Nothing
    , disableMonospaceOptimizations: Nothing
    , hideCursorInOverviewRuler: Nothing
    , scrollBeyondLastLine: Nothing
    , automaticLayout: Nothing
    , wordWrap: Nothing
    , wordWrapColumn: Nothing
    , wordWrapMinified: Nothing
    , wrappingIndent: Nothing
    , wordWrapBreakBeforeCharacters: Nothing
    , wordWrapBreakAfterCharacters: Nothing
    , wordWrapBreakObtrusiveCharacters: Nothing
    , stopRenderingLineAfter: Nothing
    , hover: Nothing
    , links: Nothing
    , contextmenu: Nothing
    , mouseWheelScrollSensitivity: Nothing
    , multiCursorModifier: Nothing
    , accessibilitySupport: Nothing
    , quickSuggestions: Nothing
    , quickSuggestionsDelay: Nothing
    , parameterHints: Nothing
    , iconsInSuggestions: Nothing
    , autoClosingBrackets: Nothing
    , autoIndent: Nothing
    , formatOnType: Nothing
    , formatOnPaste: Nothing
    , dragAndDrop: Nothing
    , suggestOnTriggerCharacters: Nothing
    , acceptSuggestionOnEnter: Nothing
    , acceptSuggestionOnCommitCharacter: Nothing
    , snippetSuggestions: Nothing
    , emptySelectionClipboard: Nothing
    , wordBasedSuggestions: Nothing
    , suggestFontSize: Nothing
    , suggestLineHeight: Nothing
    , selectionHighlight: Nothing
    , occurrencesHighlight: Nothing
    , codeLens: Nothing
    , folding: Nothing
    , showFoldingControls: Nothing
    , matchBrackets: Nothing
    , renderWhitespace: Nothing
    , renderControlCharacters: Nothing
    , renderIndentGuides: Nothing
    , renderLineHighlight: Nothing
    , useTabStops: Nothing
    , fontFamily: Nothing
    , fontWeight: Nothing
    , fontSize: Nothing
    , lineHeight: Nothing
    , letterSpacing: Nothing
    } 

newtype ScrollbarVisibility = ScrollbarVisibility String
scrollbarVisibilityAuto :: ScrollbarVisibility
scrollbarVisibilityAuto = ScrollbarVisibility "auto"

scrollbarVisibilityVisible :: ScrollbarVisibility
scrollbarVisibilityVisible = ScrollbarVisibility "visible"

scrollbarVisibilityHidden :: ScrollbarVisibility
scrollbarVisibilityHidden = ScrollbarVisibility "hidden"

type EditorScrollbarOptions = {
    -- | The size of arrows (if displayed).
    -- | Defaults to 11.
    arrowSize :: Maybe Number,

    -- | Render vertical scrollbar.
    -- | Accepted values: 'auto', 'visible', 'hidden'.
    -- | Defaults to 'auto'.
    vertical :: Maybe ScrollbarVisibility,

    -- | Render horizontal scrollbar.
    -- | Accepted values: 'auto', 'visible', 'hidden'.
    -- | Defaults to 'auto'.
    horizontal :: Maybe ScrollbarVisibility,

    -- | Cast horizontal and vertical shadows when the content is scrolled.
    -- | Defaults to true.
    useShadows :: Maybe Boolean,

    -- | Render arrows at the top and bottom of the vertical scrollbar.
    -- | Defaults to false.
    verticalHasArrows :: Maybe Boolean,

    -- | Render arrows at the left and right of the horizontal scrollbar.
    -- | Defaults to false.
    horizontalHasArrows :: Maybe Boolean,

    -- | Listen to mouse wheel events and react to them by scrolling.
    -- | Defaults to true.
    handleMouseWheel :: Maybe Boolean,

    -- | Height in pixels for the horizontal scrollbar.
    -- | Defaults to 10 (px).
    horizontalScrollbarSize :: Maybe Number,

    -- | Width in pixels for the vertical scrollbar.
    -- | Defaults to 10 (px).
    verticalScrollbarSize :: Maybe Number,

    -- | Width in pixels for the vertical slider.
    -- | Defaults to `verticalScrollbarSize`.
    verticalSliderSize :: Maybe Number,

    -- | Height in pixels for the horizontal slider.
    -- | Defaults to `horizontalScrollbarSize`.
    horizontalSliderSize :: Maybe Number
}


defaultEditorScrollbarOptions :: EditorScrollbarOptions
defaultEditorScrollbarOptions = {
    arrowSize : Nothing,
    vertical : Nothing,
    horizontal : Nothing,
    useShadows : Nothing,
    verticalHasArrows : Nothing,   
    horizontalHasArrows : Nothing, 
    handleMouseWheel : Nothing,
    horizontalScrollbarSize : Nothing,
    verticalScrollbarSize : Nothing,
    verticalSliderSize : Nothing,
    horizontalSliderSize : Nothing
}

newtype ShowSlider = ShowSlider String

showSliderAlways :: ShowSlider
showSliderAlways = ShowSlider "always"

showSliderMouseOver :: ShowSlider
showSliderMouseOver = ShowSlider "mouseover"

type EditorMinimapOptions = {
    -- | Enable the rendering of the minimap.
    -- | Defaults to false.
    enabled :: Maybe Boolean,

    -- | Control the rendering of the minimap slider.
    -- | Defaults to 'mouseover'.
    showSlider :: Maybe ShowSlider,

    -- | Render the actual text on a line (as opposed to color blocks).
    -- | Defaults to true.
    renderCharacters :: Maybe Boolean,

    -- | Limit the width of the minimap to render at most a certain number of columns.
    -- | Defaults to 120.
    maxColumn :: Maybe Int
}

defaultMinimapOptions :: EditorMinimapOptions
defaultMinimapOptions = {
    enabled : Nothing,
    showSlider : Nothing,
    renderCharacters: Nothing,
    maxColumn: Nothing
}

type EditorFindOptions = {
    -- | Controls if we seed search string in the Find Widget with editor selection.
    seedSearchStringFromSelection :: Maybe Boolean,

    -- | Controls if Find in Selection flag is turned on when multiple lines of text are selected in the editor.
    autoFindInSelection :: Maybe Boolean
}
