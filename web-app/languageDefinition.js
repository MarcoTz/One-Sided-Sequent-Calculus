//this loads the neccessary monaco-editor functions
require.config({ paths: { 'vs': 'monaco-editor/min/vs' } });

// Register a new language
require(['vs/editor/editor.main'], function () {
    monaco.languages.register({ id: 'one-sided-sequent-calculus'});
});

// for unicode '\u{"here is number"}' , '\u{3008}' -'\u{3009}' is 〈 〉
// fix bracket matching
require(['vs/editor/editor.main'], function () {
    config = {"surroundingPairs":[{"open":"{","close":"}"},{"open":"(","close":")"},{"open":"[","close":"]"},{"open":"<","close":">"},{"open":'"',"close":'"'},{"open":"'","close":"'"},
                    {"open":'\u{3008}',"close":'\u{3009}'}],
            "autoClosingPairs":[{"open":"{","close":"}"},{"open":"(","close":")"},{"open":"[","close":"]"},{"open":"<","close":">"},{"open":'"',"close":'"'},{"open":"'","close":"'"},
                    {"open":'\u{3008}',"close":'\u{3009}'}],
            "brackets":[["{","}"],["(",")"],["[","]"],
                    ['\u{3008}','\u{3009}']]}
    monaco.languages.setLanguageConfiguration('one-sided-sequent-calculus', config)
});

// Register a tokens provider for the language
require(['vs/editor/editor.main'], function () {
    monaco.languages.setMonarchTokensProvider('one-sided-sequent-calculus', {
        defaultToken: 'invalid',
        keywords: ['module','import','var', 'rec', 'main',      //Definitions
                    'CBV', 'CBN', 'CBA',                        //Kinds
                    'error', 'Done', 'Print', 'print',          //Commands
                    'if', 'then', 'else', 'case', 'forall',     //Conditions
                    'mu', 'co'                     //other, \u{03BC} has some issues with catching it
                    ],


        typeKeywords: ['data', 'codata'],

        operators: ['::',':=','=>','|','<<','>>','->','&&', '||'],

        // we include these common regular expressions
        symbols:  /[=><!~?:&|+\-*\/\^%\,;.\\]{1,2}/,

        
        tokenizer:{
            root:[

                ["module" , 'moduleDeclaration', 'moduleState'],
                ["import" , 'imports', 'moduleState'],


                //mu \u03BC abstraction
                [/((mu|\u03BC) [a-z])[\w]*[.]/, 'mu abstraction'],

                //lambda \u03BB abstraction
                [/(\\|\u03BB )[a-z][\w]*[.]/, 'lambda abstraction'],

                //unicode keyword
                [/(\u03BC|\u03BB)/, 'keyword'],

                // identifiers and keywords A-Z
                [/@?[A-Z][\w$]*/, { 
                    cases: {
                        '@typeKeywords': 'keyword.type',
                        '@keywords': {cases:{
                            'CBV|CBN|CBA' : 'Kinds',
                            // above version works too
                            //'CBN' : 'Kinds',
                            //'CBA' : 'Kinds',
                            '@default' : 'keyword'
                        }},                      
                        '@default': 'type.identifier'
                    }
                }],

                // identifiers and keywords a-z
                [/@?[a-z][\w$]*/, { 
                    cases: {
                        '@typeKeywords': 'keyword.type',
                        '@keywords': {cases:{
                            //'mu' : 'mu abstraction',
                            //'module' : 'moduleDeclaration', 
                            //'import' : 'imports',  
                            'main' : 'main',      
                            '@default' : 'keyword'
                        }},                          
                        '@default': 'identifier'
                    }
                }],

                //numbers
                [/[\d]+/, 'numbers'],

                //comments
                [/(^\-\-.*$)/, 'comment'],

                //operators
                [/@symbols/, {
                    cases: {
                        '<<|>>' : 'Kinds',
                        '@operators': 'operator',
                        '@default'  : 'symbol'
                    }
                }],

                //brackets
                [/[{}()\[\]<>]/, '@brackets'],


                //string
                [/".*?"/, 'string'],
                [/'.*?'/, 'string'],
            ],
            moduleState:[
                [/[A-Z][a-zA-Z0-9]*/, "moduleName", 'root']
            ]
        }
    });
});




// Define a new theme that contains rules that match this language
require(['vs/editor/editor.main'], function () {
    monaco.editor.defineTheme("custom-vs-dark", {
        base: "vs-dark",
        inherit: true,
        rules: [
            { token: "Kinds", foreground: "66dfed" },
            { token: "operator", foreground: "7381bf" },
            { token: "lambda abstraction", foreground: "b2cc5c" },
            { token: "mu abstraction", foreground: "d19a56" },
            { token: "moduleDeclaration", foreground: "cf56d1" },
            { token: "main", foreground: "cf56d1" },
            { token: "imports", foreground: "e8e86f" },
            { token: "moduleName", foreground: "b2b2d6" },
        ],
        colors: {
            "editor.foreground": "#ffffff",
        },
    });
})

// auto complete
require(['vs/editor/editor.main'], function () {  
    monaco.languages.registerCompletionItemProvider('one-sided-sequent-calculus', {
        provideCompletionItems: function () {

            // list of suggetstions
            const listOfSuggestions = [
                // for unicode '\u{"here is number"}' , '\u{3008}' -'\u{3009}' is 〈 〉, '\u{03BC}' is μ
                {   //Call by Any
                    label: 'CBA',
                    kind: monaco.languages.CompletionItemKind.Function,
                    documentation: "Call by any \n  \u{3008} t1 | CBA | t2 \u{3009} \nwhere t1 and t2 are terms",
                    insertText: '\u{3008} _ | CBA | _ \u{3009}',
                },
                {   //Call by Value
                    label: 'CBV',
                    kind: monaco.languages.CompletionItemKind.Function,
                    documentation: "Call by value \n  \u{3008} t1 | CBV | t2 \u{3009} \nwhere t1 and t2 are terms",
                    insertText: '\u{3008} _ | CBV | _ \u{3009}',
                },
                {   //Call by Name
                    label: 'CBN',
                    kind: monaco.languages.CompletionItemKind.Function,
                    documentation: "Call by name \n  \u{3008} t1 | CBN | t2 \u{3009} \nwhere t1 and t2 are terms",
                    insertText: '\u{3008} _ | CBN | _ \u{3009}',
                },
                
                {   //mu abstraction
                    label: 'mu-abstraction',
                    kind: monaco.languages.CompletionItemKind.Keyword,
                    documentation: "\u{03BC}-abstraction \n  \u{03BC} x. c \nwhere x is a variable and c is a command",
                    insertText: '\u{03BC} ',
                },
                {   //angle brackets
                    label: 'angle Brackets \u{3008} \u{3009}',
                    kind: monaco.languages.CompletionItemKind.Keyword,
                    documentation: "\u{3008} ... \u{3009}",
                    insertText: '\u{3008}\u{3009}',
                },
                
                

            ]

            return {
                suggestions: listOfSuggestions,
            };
        },
    });
});