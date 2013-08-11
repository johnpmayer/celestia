
/* Set up loading and executing of non-common-js modules */
var fs = require('fs');
var vm = require('vm');

var include = function(filename) {
    var data = fs.readFileSync(filename);
    var script = vm.createScript(data);
    script.runInThisContext();
}

/* Pull in the neccessary nodejs APIs and expose as global (vm executed modules can't use require) */

var EventEmitter = require('events').EventEmitter;
GLOBAL.readline = require('readline');
GLOBAL.emitter = new EventEmitter();

/* Evaluate the runtime */

include('./build/elm-runtime.js');

/* TODO Evaluate the dependencies */

/* Evaluate the main module */

include('./cache/Server.elmo');

Elm.noscreen(Elm.Server);
