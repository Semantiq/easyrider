app.service("Versions", ["Api", function(Api) {
    this.subscription = Api.subscribe("easyrider.Repository$VersionAvailableEvent", []);
    this.list = this.subscription.snapshot;
}]);