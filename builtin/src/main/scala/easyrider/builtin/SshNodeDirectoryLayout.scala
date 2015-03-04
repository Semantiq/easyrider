package easyrider.builtin

import easyrider.Applications.ContainerId

trait SshNodeDirectoryLayout {
  protected def versionsDir(containerId: ContainerId) = containerDir(containerId) + "/versions"
  protected def logDir(containerId: ContainerId) = containerDir(containerId) + "/log"
  protected def etcDir(containerId: ContainerId) = containerDir(containerId) + "/etc"
  protected def dataDir(containerId: ContainerId) = containerDir(containerId) + "/data"

  protected def containerDir(containerId: ContainerId) = {
    s"easyrider/containers/${containerId.containerName}"
  }
}
