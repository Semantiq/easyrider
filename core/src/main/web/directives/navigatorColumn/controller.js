app.directive("navigatorColumn", function() {
 	return {
 	    transclude: true,
 		restrict: "E",
 		templateUrl: "/directives/navigatorColumn/template.html",
 		scope: {
 		    label: "@",
 		    entries: "&",
 		    details: "&",
 		    add: "&"
        },
        require: '^navigator',
        link: function(scope, elem, attrs, navigator) {
            navigator.addColumn(scope);
        },
        controller: ["$scope", function($scope) {
        }]
    };
});

app.directive("transcope", function() {
    return {
        link: function($scope, $element, $attrs, controller, $transclude) {
            if (!$transclude) {
                throw minErr('ngTransclude')('orphan',
                    'Illegal use of ngTransclude directive in the template! ' +
                    'No parent directive that requires a transclusion found. ' +
                    'Element: {0}',
                    startingTag($element));
            }
            var innerScope = $scope.$new();
            $transclude(innerScope, function(clone) {
                $element.empty();
                $element.append(clone);
                $element.on('$destroy', function() {
                    innerScope.$destroy();
                });
            });
        }
    }
});
