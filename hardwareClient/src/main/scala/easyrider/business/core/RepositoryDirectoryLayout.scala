package easyrider.business.core

import java.io.File

trait RepositoryDirectoryLayout {
  val easyriderData: File
  val repositoryDir = new File(easyriderData, "repository")
}
