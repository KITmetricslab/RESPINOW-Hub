import git
import pandas as pd

repo = git.Repo("../")
tree = repo.tree()

commit_dates = pd.DataFrame(columns=["filename", "first_commit", "latest_commit"])

for blob in tree.traverse():
    if blob.path.startswith("submissions") & blob.path.endswith(".csv"):
        commits = list(repo.iter_commits(paths=blob.path))
        commit_dates.loc[len(commit_dates)] = [
            blob.path.split("/")[-1],  # filename
            str(
                pd.to_datetime(commits[-1].committed_date, unit="s").date()
            ),  # first commit
            str(pd.to_datetime(commits[0].committed_date, unit="s").date()),
        ]  # latest commit


commit_dates[["forecast_date", "source", "indicator", "model"]] = commit_dates[
    "filename"
].str.extract(r"^(\d{4}-\d{2}-\d{2})-([^-]+)-([^-]+)-([^.]+)")

commit_dates["forecast_date"] = pd.to_datetime(commit_dates.forecast_date)
commit_dates["latest_commit"] = pd.to_datetime(commit_dates.latest_commit)
commit_dates["diff"] = commit_dates.latest_commit - commit_dates.forecast_date
commit_dates["retrospective"] = commit_dates["diff"] > pd.Timedelta(days=3)

commit_dates.to_csv(
    "../respinow_viz/plot_data/other/list_commit_dates.csv", index=False
)
