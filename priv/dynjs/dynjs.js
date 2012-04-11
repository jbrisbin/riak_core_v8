var JSON = require("json");
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

  exports.handle_exit = function() {
    return core.stop("stopping");
  }

  exports.handle_handoff_command = function(msg) {
    return core.OK;
  }

  exports.handoff_starting = function(target) {
    return true;
  }

  exports.handoff_cancelled = function() {
    return core.OK;
  }

  exports.handoff_finished = function(target) {
    return core.OK;
  }

  exports.handle_handoff_data = function(data) {
    return core.reply(core.OK);
  }

  exports.encode_handoff_item = function(name, val) {
    return JSON.stringify({ 
      "name": name,
      "value": val
    });
  }

})();
