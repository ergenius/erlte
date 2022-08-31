/**
 * Validator
 */
 erlte.S.Utils.Validator = {
    
    /**
     * Returns true if the specified parameter is string
     * @param {any} param Parameter to check
     * @return {boolean} true if param is string or false otherwise
     * @function
     */
    isString: function (param) {
        return typeof param === 'string' || param instanceof String;
    },

    /**
     * Returns true if the specified parameter is non empty string
     * @param {any} param Parameter to check
     * @return {boolean} true if param is non empty string or false otherwise
     * @function
     */
    isStringNotEmpty: function (param) {
        return ((typeof param === 'string' || param instanceof String) && param !== '');
    }
    
}