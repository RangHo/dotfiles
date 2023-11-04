<img align="center" src="verniy-dotfiles.png" />
<p align="center">
  <small>Original image by <a href="https://www.pixiv.net/member_illust.php?id=3180989">キノスケ</a></small>
</p>

--------------------------------------------------------------------------------

## What is a _dotfile_?

Dotfiles are similar to what Windows users would call *hidden files*.
They are often used to customize UNIX systems. The name **dotfiles** comes from
a UNIX convention where hidden file's name starts with a `.`(dot).
This convention traces its origin back to old UNIX where (by unintentional side
effect) files starting with `.` became invisible by some programs.
People exploited this "bug" to make files hidden, and the UNIX community finally
adopted this as a "feature". Remember: *it's not a bug; it's a feature*.

## How to use

Though it isn't made to install dotfiles on systems, `GNU stow` provides a nice
interface for installing dotfiles by creating symbolic links automatically.
But, I, having too much free time that I should've spent doing something far
more useful, created a "package manager" that installs binary dependencies as
well.

First, clone the repository into your favorite location. Don't forget to
initialize submodules as some features depend on submodules.

```sh
git clone --recurse-submodules https://github.com/RangHo/dotfiles ~/Dotfiles
```

Then `cd` into the directory you chose.

```sh
cd ~/Dotfiles
```

Now you should initialize dotfile manager by running this script below.
Provide `sudo` password if necessary.

```sh
./dotfile init
```

By now, the `dotfile` script should have installed itself in `~/.local/bin`
directory.
It is just for convenient access, so you may still use the script directly.

To install a dotfile package, run the following command:

```sh
dotfile install <package>
# ... where <package> is a directory that contains `pkginfo` file
```

## List of commands

Here are complete list of commands:

| Command | What it does |
|-----|-----|
| `dotfile install <package>` | Installs a package named `<package>`. It installs dependencies too. |
| `dotfile uninstall [--force] <package>` | Uninstalls a package named `<package>`. Note that this does not uninstall binary dependencies. |
| `dotfile update` | Updates the dotfiles repository. This feature is not tested rigorously, so use with caution. |
| `dotfile remove` | Remove all packages from the machine. It does not guarantee that all artifacts are removed also. |

## Screenshots

![image](https://user-images.githubusercontent.com/10833976/113498541-ce85ef00-9548-11eb-847d-b40af369fbe8.png)
![image](https://user-images.githubusercontent.com/10833976/113498569-160c7b00-9549-11eb-9944-34e362aa4c4b.png)
![image](https://user-images.githubusercontent.com/10833976/113498617-756a8b00-9549-11eb-8907-8b814e313ec3.png)
