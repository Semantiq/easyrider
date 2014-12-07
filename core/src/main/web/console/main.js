// TODO: implement or drop
var repl = require("repl");
var api = require("./api.js").api;
var r = repl.start({
 prompt: ">> ",
 ignoreUndefined: true
});
r.context.easyrider = "easyrider";
r.context.api = api;
