app.service("Search", ["SearchGrammar", function(SearchGrammar) {
	this.Query = function() {
		var me = this;
		me.query = "";
		me.typeahead = [];
		me.results = [];
		function processParseResults(ast) {
			console.log(ast);
			me.typeahead = [ast];
		}
		me.process = function() {
			var ast;
			try {
				ast = SearchGrammar.parse(me.query);
			} catch(e) {
				me.typeahead = [{ error: e }];
				me.results = [];
			}
			if(ast)
				processParseResults(ast);
		};
	};
}]);