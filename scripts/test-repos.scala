//> using dep org.kohsuke:github-api:1.324
//> using dep com.lihaoyi::pprint::0.9.0
//> using dep ch.epfl.lamp::gears::0.2.0
//> using dep com.outr::scribe::3.15.0
//> using toolkit default
//> using scala 3
//> using jvm 21

import gears.async.*
import gears.async.Retry.Delay
import gears.async.Retry.Jitter
import gears.async.default.given
import org.kohsuke.github.GHFileNotFoundException
import org.kohsuke.github.GHIssueState
import org.kohsuke.github.GHPullRequestQueryBuilder
import org.kohsuke.github.GHRepository
import org.kohsuke.github.GitHubBuilder

import java.util.Date
import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.kohsuke.github.GHException
import org.kohsuke.github.HttpException
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val inactiveYears = 2
val inactiveDateCutoff =
  val date = Date()
  date.setYear(date.getYear() - inactiveYears)
  date

val lastMonth =
  val date = LocalDate.now().minusDays(30)
  ">" + date.format(DateTimeFormatter.ISO_LOCAL_DATE)

val renamedMessage =
  "The listed users and repositories cannot be searched either because the resources do not exist or you do not have permission to view them."

case class RepoEntry(repo: String, branch: Option[String]):
  override def toString(): String = this match
    case RepoEntry(repo, Some(branch)) => s"$repo:$branch"
    case RepoEntry(repo, None)         => repo

enum RepoResult(repo: String, branch: Option[String], val isValid: Boolean):
  case Invalid(repo: String, branch: Option[String])
      extends RepoResult(repo, branch, isValid = false)
  case Archived(repo: String)
      extends RepoResult(repo, branch = None, isValid = false)
  case Correct(entry: RepoEntry)
      extends RepoResult(entry.repo, entry.branch, isValid = true)
  case Inactive(repo: String)
      extends RepoResult(repo, branch = None, isValid = false)
  case Stale(repo: String)
      extends RepoResult(repo, branch = None, isValid = false)
  case Renamed(repo: String)
      extends RepoResult(repo, branch = None, isValid = false)
  case Failure(repo: String, exception: Throwable)
      extends RepoResult(repo, branch = None, isValid = false)

  override def toString(): String =
    this match
      case Invalid(repo, None)         => repo + " (doesn't exist)"
      case Invalid(repo, Some(branch)) => s"$repo:$branch (no such branch)"
      case Stale(repo)                 => s"$repo no recent Scala Steward prs"
      case Archived(value)             => s"$repo is archived"
      case Renamed(value)              => s"$repo might be renamed"
      case Inactive(value) => s"$repo is inactive for last $inactiveYears"
      case Correct(_)      => s"$repo:$branch (OK)"
      case Failure(_, e)   => s" Failed to query $repo -> ${e.getMessage()}"

/** Potential improvements:
  *   - find active repositories with no opened Scala Steward PRs (potential
  *     issue)
  * Run with `scala-cli ./scripts/test-repos.scala -- $GITHUB_TOKEN `
  * @param githubToken
  */
@main
def main(
    githubToken: String
) =
  val reposGihub = os.pwd / "repos-github.md"
  val repos = os
    .read(reposGihub)
    .split("\n")
    .toList
    .flatMap { repo =>
      repo.trim().stripPrefix("-").trim().split(":") match
        case Array(repo)         => Some(RepoEntry(repo, None))
        case Array(repo, branch) => Some(RepoEntry(repo, Some(branch)))
        case _                   => None

    }
  val gh = new GitHubBuilder()
    .withOAuthToken(githubToken)
    .build()

  def wrongBranch(ghRepo: GHRepository, repoEntry: RepoEntry) =
    repoEntry match
      case RepoEntry(repo, Some(branch)) =>
        Try(ghRepo.getBranch(branch)) match
          case Failure(exception) =>
            Some(RepoResult.Invalid(repoEntry.repo, Some(branch)))
          case Success(value) =>
            None
      case _ => None

  def archived(ghRepo: GHRepository, repoEntry: RepoEntry) =
    if ghRepo.isArchived() then Some(RepoResult.Archived(repoEntry.repo))
    else None

  def isInactive(ghRepo: GHRepository, repoEntry: RepoEntry) =
    val lastCommit = ghRepo.getBranch(ghRepo.getDefaultBranch()).getSHA1()
    val lastCommitDate = ghRepo.getCommit(lastCommit).getCommitDate()
    if lastCommitDate.before(inactiveDateCutoff) then
      Some(RepoResult.Inactive(repoEntry.repo))
    else None

  def noScalaStewardActivity(ghRepo: GHRepository, repoEntry: RepoEntry) =

    val hasStewardRequests =
      gh.searchIssues()
        .q(
          s"repo:${repoEntry.repo} type:pr author:scala-steward created:$lastMonth"
        )
        .list()
        .withPageSize(1)
        .iterator()
        .hasNext()

    if hasStewardRequests then None
    else Some(RepoResult.Stale(repoEntry.repo))

  Async.blocking:
    val nonExistent = repos
      .grouped(5)
      .flatMap { repos =>
        repos.map { repoEntry =>
          val request = Future:
            println(s"Analysing $repoEntry")
            Try(gh.getRepository(repoEntry.repo)) match
              case Failure(exception: GHFileNotFoundException) =>
                RepoResult.Invalid(repoEntry.repo, repoEntry.branch)
              case Success(ghRepo) =>
                wrongBranch(ghRepo, repoEntry)
                  .orElse(archived(ghRepo, repoEntry))
                  .orElse(isInactive(ghRepo, repoEntry))
                  .orElse(noScalaStewardActivity(ghRepo, repoEntry))
                  .getOrElse(RepoResult.Correct(repoEntry))
              case _ => RepoResult.Correct(repoEntry)

          Retry.untilSuccess
            .withMaximumFailures(5)
            .withDelay(
              Delay.backoff(
                maximum = 5.minutes,
                starting = 10.seconds,
                multiplier = 4,
                jitter = Jitter.equal
              )
            ) {
              request.awaitResult match
                case Failure(exception: GHException) =>
                  Option(exception.getCause()) match
                    case Some(http: HttpException)
                        if http.getMessage().contains(renamedMessage) =>
                      RepoResult.Renamed(repoEntry.repo)
                    case _ =>
                      scribe.error(
                        s"Unexpeected error when querying ${repoEntry.repo}",
                        exception
                      )
                      RepoResult.Failure(repoEntry.repo, exception)
                case Failure(exception) =>
                  scribe.error(
                    s"Unexpeected error when querying ${repoEntry.repo}",
                    exception
                  )
                  RepoResult.Failure(repoEntry.repo, exception)
                case Success(value) => value
            }
        }
      }
      .filter(!_.isValid)
      .toList

    if nonExistent.nonEmpty then
      println(s"\nIdentified problems with ${nonExistent.length} repositories:")
      nonExistent.foreach: repo =>
        println(s" - https://github.com/$repo")
