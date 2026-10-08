module Config.Test

import Config

import TTest

namespace ParseGitHubURI
  parseSSHGithubDotCom : parseGitHubURI "git@github.com:mattpolzin/harmony"
                           ==> Just (Remote "github.com" "mattpolzin" "harmony")
  parseSSHGithubDotCom = MkTTest

  parseHTTPSGithubDotCom : parseGitHubURI "https://github.com/mattpolzin/harmony"
                             ==> Just (Remote "github.com" "mattpolzin" "harmony")
  parseHTTPSGithubDotCom = MkTTest

  parseSSHOtherDomain : parseGitHubURI "git@git.my-domain.com:mattpolzin/harmony"
                          ==> Just (Remote "git.my-domain.com" "mattpolzin" "harmony")
  parseSSHOtherDomain = MkTTest

  parseHTTPSOtherDomain : parseGitHubURI "https://git.my-domain.com/mattpolzin/harmony"
                            ==> Just (Remote "git.my-domain.com" "mattpolzin" "harmony")
  parseHTTPSOtherDomain = MkTTest

  parseSSHGithubDotComExt : parseGitHubURI "git@github.com:mattpolzin/harmony.git"
                              ==> Just (Remote "github.com" "mattpolzin" "harmony")
  parseSSHGithubDotComExt = MkTTest

  parseHTTPSGithubDotComExt : parseGitHubURI "https://github.com/mattpolzin/harmony.git"
                              ==> Just (Remote "github.com" "mattpolzin" "harmony")
  parseHTTPSGithubDotComExt = MkTTest

  parseSSHBroken : parseGitHubURI "git@github.com/mattpolzin/harmony"
                     ==> Nothing
  parseSSHBroken = MkTTest

  parseHTTPSBroken : parseGitHubURI "https://github.com:mattpolzin/harmony"
                       ==> Nothing
  parseHTTPSBroken = MkTTest

