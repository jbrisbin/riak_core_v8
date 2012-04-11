exports.sayHello = function(obj) { return "Hello " + obj.name + "!"; }

exports.TestObj = function() {}
exports.TestObj.prototype.saySomething = function() { 
  return "Something!"; 
}