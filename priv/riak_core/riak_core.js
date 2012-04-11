var OK = "ok";
var CONTINUE = "continue";

exports.OK = OK;
exports.CONTINUE = CONTINUE;

(function() {
  
  exports.reply = function(msg) { return OK; }  
  exports.stop = function() { return OK; }
  
})();
