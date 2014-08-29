app.directive("uploadVersion", function() {
	return {
        restrict: "E",
		templateUrl: "/directives/uploadVersion/template.html",
        scope: {
            applicationId: "=appid"
        },
		controller: ["$scope", "$upload", "$location", function($scope, $upload, $location) {
		    $scope.upload = { };
		    $scope.uploading = false;
		    $scope.start = function() {
                if(!$scope.files || !$scope.files[0])
                    return;
		        $scope.uploading = true;
                $scope.uploadProgress = 0;
                var upload = $upload.upload({
                    url: "/api/repository/upload",
                    headers: {
                        //"Authorization": ("Basic " + btoa($scope.username + ":" + $scope.password))
                    },
                    data: {
                        application: $scope.applicationId.id,
                        version: $scope.upload.version
                    },
                    file: $scope.files[0],
                    fileFormDataName: "content"
                });
                upload.progress(function(event) {
                    $scope.uploadProgress = 100 * event.loaded / event.total;
                });
                upload.success(function(data) {
                   $location.path("/application/" + $scope.applicationId.id + "/version/" + $scope.upload.version);
                });
		    };
		}]
	};
});