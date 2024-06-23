"use strict";


//new options mapping function, based on what options looks like
var mapMaybes = function (options) {
    var newObj = {};
    for (prop in options) {
        var propKey = prop;
        var propValue = options[prop];
        var newValue = undefined;
        if (Object.keys(propValue).length != 0) {
            newValue = propValue["value0"]
            newObj[propKey] = newValue;
        }
    }
    return newObj;
}


//here is the new implementation using getElementbyId
export const createImpl = function (options) {
    return function (el) {
        return function () {
            return new Promise(function (resolve, reject) {               
                // make it happy for browser env
                monacoRequire.config({ paths: { 'vs': 'monaco-editor/min/vs' } });
                
                monacoRequire(['vs/editor/editor.main'], function () {
                    //console.log(options);
                    const mappedOpts = mapMaybes(options);
                    //console.log(mappedOpts);
                    //console.log(el);
                    window.editorVar = monaco.editor.create(document.getElementById(el), mappedOpts);
                    //console.log(editor);
                    resolve(window.editorVar);
                });
            });
        };
    };
};


//2 new functions to import that get/set values for the editor
export const readEditorValue = function (){
    return window.editorVar.getValue()
}

export const setEditorValue = function (strValue){
    window.editorVar.setValue(strValue)
}