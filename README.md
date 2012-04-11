# V8-based VNode Executor

The idea here is to embed the V8 Javascript VM (via [erlv8](https://github.com/beamjs/erlv8)) into 
a riak_core vnode such that one could export a set of functions as a [CommonJS](http://www.commonjs.org/) 
module and have the riak_core vnode dispatch calls into this Javascript module.

An example Javascript file might look like:

    var core = require("riak_core");

    (function() {
  
      exports.handle_command = function(msg) {
        return core.reply({
          "result": "result value"
        });
      }

      exports.handle_info = function(msg) {
        return core.OK;
      }

      exports.handle_coverage = function(req, keySpaces) {
        return core.CONTINUE;
      }

    })();

The vnode that receives the "handle_command" message will pass it along to this Javascript function, 
turning any arguments into a Javascript object.