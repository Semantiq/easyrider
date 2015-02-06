app.service("Versions", ["Api", function(Api) {
    var me = this;
    me.versionsByApplicationId = {};
    me.versions = Api.subscribeSnapshot("easyrider.Repository$VersionMetadata", function(snapshot) {
        angular.forEach(snapshot.entries, function(value, key) {
            var applicationId = value.version.applicationId.id;
            if (!me.versionsByApplicationId[applicationId]) {
                me.versionsByApplicationId[applicationId] = [];
            }
            me.versionsByApplicationId[applicationId].push(value);
        });
    });
}]);
