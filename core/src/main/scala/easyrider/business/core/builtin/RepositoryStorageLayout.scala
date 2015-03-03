package easyrider.business.core.builtin

import java.io.File

import easyrider.Repository.Version

object RepositoryStorageLayout {
  def applicationDir(repositoryDir: File, version: Version) = new File(repositoryDir, version.applicationId.id)
  def versionFileName(repositoryDir: File, version: Version) = new File(applicationDir(repositoryDir, version), version.number + ".tar.bz2")
}
