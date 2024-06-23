//this loads the neccessary monaco-editor functions
require.config({ paths: { 'vs': 'monaco-editor/min/vs' } });

// Register a new language
require(['vs/editor/editor.main'], function () {
    monaco.languages.register({ id: 'one-sided-sequent-calculus'});
});

// Register a tokens provider for the language
// currently just a test (based on examples) to see if implementation works
require(['vs/editor/editor.main'], function () {
    monaco.languages.setMonarchTokensProvider('one-sided-sequent-calculus', {
        defaultToken: 'invalid',
        keywords: ['module','import','data','codata','var','Print','Done','error',
                    'case', 'forall', 'CBV', 'CBN', 'mu', 'co',],
        operators: ['::',':=','=>','|','<<','>>',':'],
        // we include these common regular expressions
        symbols:  /[=><!~?:&|\-+*\/\^%]+/,
        tokenizer:{
            root:[

                // identifiers and keywords
                [/@?[a-zA-Z][\w$]*/, { 
                    cases: {
                        '@keywords': 'keyword',
                        //'@default': 'identifier'
                    }
                }],

                //brackets
                [/[{}()\[\]<>]/, '@brackets'],

                //comments
                //somehow the - gets caught by @symbols and it doesnt work anymore | it works if the order is swapped
                [/(^\-\-.*$)/, 'comment'],

                //operators
                [/@symbols/, {
                    cases: {
                        '@operators': 'operator',
                        '@default'  : ''
                    }
                }],

                //string
                [/".*?"/, 'string'],
            ]
        }
    });
});