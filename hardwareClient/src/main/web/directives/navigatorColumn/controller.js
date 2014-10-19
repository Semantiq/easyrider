app.directive("navigatorColumn", function() {
 	return {
 	    transclude: true,
 		restrict: "E",
 		templateUrl: "/directives/navigatorColumn/template.html",
 		scope: {
 		    label: "@"
        },
        require: '^navigator',
        link: function(scope, elem, attrs, navigator) {
            navigator.addColumn(scope);
        }
    };
});
