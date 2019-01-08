![Dotfiles](verniy-dotfiles.png)
Original image by [キノスケ](https://www.pixiv.net/member_illust.php?id=3180989)

# What is `dotfile`?

Dotfiles are similar to what Windows users would call *hidden files*. They are often used to customize UNIX systems. They are called **dotfiles** because in UNIX systems, hidden file's name starts with a `.`(dot). This convention traces its origin back to old UNIX where (by unintentional side effect) files starting with `.` became invisible by some programs. People exploited this "bug" to make files hidden, and the UNIX community finally adopted this as a "feature". Remember, *it's not a bug; it's a feature*.

## How to use

Using `GNU stow`, we can manage the dotfiles easily. Though `stow` itself is not designed to be used with dotfiles, it works perfectly well. To add more functionalities, I wrapped `stow` with `make` so that I can automatically initialize and finalize installation.

First, clone the repository to your favorite location. I will download it into `~/Dotfiles`.

```sh
git clone --recurse-submodules https://github.com/RangHo/dotfiles ~/Dotfiles
```

I used `~/Dotfiles` for consistent feel with existing XDG directory structure. Recursively initialize submodules to load some external themes and stuff. Then `cd` into the directory you chose.

```sh
cd ~/Dotfiles
```

Then we can install the configurations! Use this syntax to install a package: `make install=<package>`. Here, I will install `firefox`.

```sh
make install=firefox
```

## List of commands

Here are complete list of commands:

| Command | What it does |
|-----|-----|
| `make install=<package>` | Installs a package named `<package>`. It runs initialize and finalize scripts too. |
| `make uninstall=<package>` | Just an alias to `stow -t ~ -D <package>`. Uninstalls a package. |
| `make update` | Updates the dotfiles repository. This first uninstalls all available configuration. You have to reinstall packages later. |
| `make remove` | Remove all packages from the machine. It cannot guarantee that all artifacts are removed also. }
