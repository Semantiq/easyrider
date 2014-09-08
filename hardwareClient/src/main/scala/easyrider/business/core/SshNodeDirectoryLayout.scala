package easyrider.business.core

import easyrider.Applications.ContainerId

trait SshNodeDirectoryLayout {
  protected def versionsDir(containerId: ContainerId) = {
    containerDir(containerId) + "/versions"
  }

  protected def containerDir(containerId: ContainerId) = {
    s"easyrider/containers/${containerId.containerName}"
  }
}
