var OK = "ok";
var CONTINUE = "continue";

exports.OK = OK;
exports.CONTINUE = CONTINUE;

(function() {
  
  exports.reply = function(msg) { return msg; }  
  exports.stop = function() { return OK; }
  
})();
