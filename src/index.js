'use strict';

require("./styles.scss");

const {
    Elm
} = require('./Anagram');
var app = Elm.Anagram.init({
    node: document.getElementById("anagram")
});