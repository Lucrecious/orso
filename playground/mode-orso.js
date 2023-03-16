ace.define("ace/mode/orso_highlighting_rules", ["require", "exports", "module", "ace/lib/oop"], function (require, exports, module) {
  "use strict";

  var oop = require("../lib/oop");
  var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

  var OrsoHighlightRules = function () {
    var keywords = ("not|if|else|unless");
    var builtinTypes = (
      "string|i32|i64|f64|symbol|void|bool"
    );
    var builtinFunctions = (
      "print_expr"
    );
    var builtinConstants = ("null|true|false");

    var keywordMapper = this.createKeywordMapper({
      "keyword": keywords,
      "constant.language": builtinConstants,
      "support.function": builtinFunctions,
      "support.type": builtinTypes
    }, "");

    this.$rules = {
      "start": [
        {
          token: "string", // single line
          regex: /"(?:[^"\\]|\\.)*?"/
        },
        {
          token: "constant.numeric", // integer
          regex: "-?\\d+"
        },
        {
          token: "constant.numeric", // float
          regex: "[-]?\\d+\\.\\d+"
        },
        {
          token: function (val) {
            return keywordMapper(val) || "identifier";
          },
          regex: "[a-zA-Z_$][a-zA-Z0-9_$]*\\b\\(?"
        }, {
          token: "keyword.operator",
          regex: "==|=|!=|<=|>=|<|>"
        }, {
          token: "punctuation.operator",
          regex: "\\:|\\;|\\|"
        }, {
          token: "paren.lparen",
          regex: "[({]"
        }, {
          token: "paren.rparen",
          regex: "[)}]"
        }, {
          token: "text",
          regex: "\\s+"
        }
      ],
    };
  };
  oop.inherits(OrsoHighlightRules, TextHighlightRules);

  exports.OrsoHighlightRules = OrsoHighlightRules;
});

ace.define("ace/mode/matching_brace_outdent", ["require", "exports", "module", "ace/range"], function (require, exports, module) {
  "use strict";
  var Range = require("../range").Range;
  var MatchingBraceOutdent = function () { };
  (function () {
    this.checkOutdent = function (line, input) {
      if (!/^\s+$/.test(line))
        return false;
      return /^\s*\}/.test(input);
    };
    this.autoOutdent = function (doc, row) {
      var line = doc.getLine(row);
      var match = line.match(/^(\s*\})/);
      if (!match)
        return 0;
      var column = match[1].length;
      var openBracePos = doc.findMatchingBracket({ row: row, column: column });
      if (!openBracePos || openBracePos.row == row)
        return 0;
      var indent = this.$getIndent(doc.getLine(openBracePos.row));
      doc.replace(new Range(row, 0, row, column - 1), indent);
    };
    this.$getIndent = function (line) {
      return line.match(/^\s*/)[0];
    };
  }).call(MatchingBraceOutdent.prototype);
  exports.MatchingBraceOutdent = MatchingBraceOutdent;

});

ace.define('ace/mode/orso', ["require", "exports", "module", "ace/lib/oop", "ace/mode/text_highlight_rules", "ace/mode/orso_highlighting_rules", "ace/mode/matching_brace_outdent", "ace/worker/worker_client"],
  function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    // defines the parent mode
    var TextMode = require("./text").Mode;
    var Tokenizer = require("../tokenizer").Tokenizer;
    var MatchingBraceOutdent = require("./matching_brace_outdent").MatchingBraceOutdent;
    var WorkerClient = require("../worker/worker_client").WorkerClient;

    // defines the language specific highlighters and folding rules
    var OrsoHighlightRules = require("./orso_highlighting_rules").OrsoHighlightRules;

    var Mode = function () {
      this.HighlightRules = OrsoHighlightRules;
      this.$outdent = new MatchingBraceOutdent();
    };
    oop.inherits(Mode, TextMode);

    (function () {
      this.getNextLineIndent = function (state, line, tab) {
        var indent = this.$getIndent(line);
        if (state == "start") {
          var match = line.match(/^.*[\{]\s*$/);
          if (match) {
            indent += tab;
          }
        }
        return indent;
      };

      this.checkOutdent = function (state, line, input) {
        return this.$outdent.checkOutdent(line, input);
      };

      this.autoOutdent = function (state, doc, row) {
        this.$outdent.autoOutdent(doc, row);
      };

      // create worker for live syntax checking
      this.createWorker = function (session) {
        var worker = new WorkerClient(["ace"], "ace/mode/orso_worker", "OrsoWorker");
        worker.attachToDocument(session.getDocument());
        worker.on("errors", function (e) {
          session.setAnnotations(e.data);
        });
        return worker;
      };
      this.$id = "ace/mode/orso";

    }).call(Mode.prototype);

    exports.Mode = Mode;
  });
