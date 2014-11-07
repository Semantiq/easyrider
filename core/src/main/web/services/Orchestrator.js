app.service("Orchestrator", ["Validators", function(Validators) {
    var me = this;

    me.releaseVersionTemplate = function(stageId) {
        return {
			jsonClass: "easyrider.Orchestrator$ReleaseVersionToStage",
			stageId: stageId,
			version: {
			    applicationId: stageId.applicationId,
			    number: ""
			}
		};
    };

    Validators.addValidator("easyrider.Orchestrator$ReleaseVersionToStage",
        function(command, reporter) {
        if(!command.version.number) {
            reporter.fail("Select version number", "Please select version number to be released");
        }
    });
}]);
