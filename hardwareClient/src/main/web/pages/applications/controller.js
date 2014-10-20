app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/applications", {
		templateUrl: "/pages/applications/template.html",
		controller: ["$scope", "Applications", "Command", "Stages", "ContainersConfiguration", "Versions", "hotkeys", "$location", function($scope, Applications, Command, Stages, ContainersConfiguration, Versions, hotkeys, $location) {
		    $scope.columns = [ "apps", "stages", "nodes" ];
		    $scope.entries = [
                Applications.list,
                [],
                []
		    ];
            $scope.selectedRowIndexes = [0, 0, 0];
		    $scope.selectedColumnIndex = 0;
		    $scope.isSelected = function(columnIndex, index) {
                return $scope.selectedRowIndexes[columnIndex] == index;
		    };
		    $scope.isColumnSelected = function(columnIndex) {
                return $scope.selectedColumnIndex == columnIndex;
		    };
		    $scope.getApplications = function(any) {
		        return Applications.list;
		    };
		    hotkeys.bindTo($scope).add({
                combo: "left",
                description: "Navigate left",
                callback: function() {
                    $scope.selectedColumnIndex = Math.max(0, $scope.selectedColumnIndex - 1);
                    $scope.onMove();
                }
		    }).add({
                combo: "right",
                description: "Navigate right",
                callback: function() {
                    var currentColumn = $scope.selectedColumnIndex;
                    $scope.selectedColumnIndex = Math.min($scope.columns.length - 1, $scope.selectedColumnIndex + 1);
                    if (currentColumn != $scope.selectedColumnIndex) {
                        if ($scope.entries[$scope.selectedColumnIndex].length === 0) {
                            $scope.selectedColumnIndex = currentColumn;
                        }
                    }
                    $scope.onMove();
                }
		    }).add({
                combo: "up",
                description: "Navigate up",
                callback: function() {
                    var newIndex = $scope.selectedRowIndexes[$scope.selectedColumnIndex] - 1;
                    $scope.selectedRowIndexes[$scope.selectedColumnIndex] = Math.max(newIndex, 0);
                    $scope.onMove();
                }
		    }).add({
                combo: "down",
                description: "Navigate down",
                callback: function() {
                    $scope.selectedRowIndexes[$scope.selectedColumnIndex] = Math.min($scope.selectedRowIndexes[$scope.selectedColumnIndex] + 1, $scope.entries[$scope.selectedColumnIndex].length - 1);
                    $scope.onMove();
                }
		    }).add({
		        combo: "enter",
                description: "See details of selected item",
                callback: function() {
                    // TODO
                    $location.path("/application/app/stage/dev");
                }
		    });
		    $scope.onMove = function() {
		        var selectedItem = $scope.entries[$scope.selectedColumnIndex][$scope.selectedRowIndexes[$scope.selectedColumnIndex]];
                if ($scope.selectedColumnIndex === 0) {
                    $scope.entries[1] = Stages.stagesOfApplication(selectedItem.application.id);
                    $scope.entries[2] = [];
                } else if ($scope.selectedColumnIndex === 1) {
                    $scope.entries[2] = ContainersConfiguration.containersInStage(selectedItem.stage.id);
                }
		    };
			$scope.Applications = Applications;
			$scope.Stages = Stages;
			$scope.Versions = Versions;

			$scope.addApplication = function() {
				Command.show(Applications.addApplicationTemplate());
			};
			$scope.removeApplication = function(id) {
				Command.show(Applications.removeApplicationTemplate(id));
			};
			$scope.addStage = function(id) {
				Command.show(Stages.addStageTemplate(id));
			};
			$scope.versionsForApplication = function(appId, versions) {
			    var filtered = [];
			    for (var i in versions) {
			        var versionEvent = versions[i];
			        if (appId.id == versionEvent.eventDetails.eventKey.key[0])
			            filtered.push(versionEvent);
			    }
			    return filtered;
			};
		}]
	});
}]);