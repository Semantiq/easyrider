app.constant("StatusClasses", {
    'easyrider.Infrastructure$CreatingNode$': {
        state: 'pending',
        label: 'Creating node'
    },
    'easyrider.Infrastructure$NodeCreated$': {
        state: 'success',
        label: 'Ok'
    },
    'easyrider.Infrastructure$ContainerCreationFailed$': {
        state: 'failure',
        label: 'Can not create container'
    },
    'easyrider.Infrastructure$ContainerCreated$': {
        state: 'paused',
        label: 'Not running'
    },
    'easyrider.Infrastructure$ContainerRunning': {
        state: 'success',
        label: 'Running'
    },
    'easyrider.Infrastructure$ContainerStopping': {
        state: 'pending',
        label: 'Stopping'
    },
    'easyrider.Infrastructure$ContainerForceStopping': {
        state: 'pending',
        label: 'Force stopping'
    },
    'easyrider.Infrastructure$DeploymentInProgress$': {
        state: 'pending',
        label: 'Deployment in progress'
    },
    'easyrider.Infrastructure$DeploymentFailed': {
        state: 'failure',
        label: 'Deployment failed'
    },
    'easyrider.Infrastructure$UnDeploymentInProgress$': {
        state: 'pending',
        label: 'Un-deployment in progress'
    },
    'easyrider.Infrastructure$DeploymentCompleted$': {
        state: 'success',
        label: 'Deployment completed'
    },
    'easyrider.Infrastructure$UnDeployed$': {
        state: 'success',
        label: 'Un-deployed'
    },
    'easyrider.Infrastructure$DeploymentFailed$': {
        state: 'success',
        label: 'Deployment failed'
    },
    'unknown': {
        state: 'unknown',
        label: 'Unknown'
    }
});