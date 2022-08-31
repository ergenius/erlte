import "./config"; // You can import without .js file extension
import "./erlte";
import "./utils/base.js"; // You can import with file extension
import "./utils/validator";

// Now let's call a function
erlte.S.Utils.Base.erlang();
erlte.S.Utils.Validator.isString("Erlang");