var v = require("./tpl.js");

v.main()

obj = v.mkCode();

obj.consume("foobar");
obj.consume("foobar");
obj.consume("foobar");
obj.produce()
