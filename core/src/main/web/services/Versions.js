app.service("Versions", ["Api", function(Api) {
    var me = this;
    me.versionsByApplicationId = {};
    me.versions = Api.subscribeSnapshot("easyrider.Repository$VersionMetadata", function(snapshot) {
        for (var member in me.versionsByApplicationId) {
            delete me.versionsByApplicationId[member];
        }
        angular.forEach(snapshot.entries, function(value, key) {
            var applicationId = value.version.applicationId.id;
            if (!me.versionsByApplicationId[applicationId]) {
                me.versionsByApplicationId[applicationId] = [];
            }
            me.versionsByApplicationId[applicationId].push(value);
        });
    });
    me.addLabelTemplate = function(version) {
        return {
            jsonClass: "easyrider.Repository$AddLabel",
            version: version,
            name: ""
        };
    };
    me.deleteVersionTemplate = function(version) {
        return {
            jsonClass: "easyrider.Repository$DeleteVersion",
            version: version
        };
    };
}]);
