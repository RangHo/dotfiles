![Dotfiles](verniy-dotfiles.png)
Original image by [キノスケ](https://www.pixiv.net/member_illust.php?id=3180989)

# What is `dotfile`?

Dotfiles are used to customize UNIX systems. They are called `dotfile`s because many configuration file names start with a `.`(dot). This convention traces its origin back to old UNIX where (by bug) files starting with `.` became invisible. People exploited this "bug" to make files hidden, and the UNIX community finally adopted this as a "feature". Basically, this is one of the biggest examples of *it's-not-a-bug-it's-a-feature* stuff.

## How to use

Using `GNU stow`, we can manage the dotfiles easily. Though `stow` itself is not designed to be used with dotfiles, it works perfectly well.

First, clone the repository to your favorite location.

```sh
git clone --recurse-submodules https://github.com/RangHo/dotfiles ~/Dotfiles
```

I used `~/Dotfiles` for consistent feel with existing directory structure. Recursively initialize submodules to load some external themes and stuff. Then `cd` into the directory you chose.

```sh
cd ~/Dotfiles
```

Now you can `stow` anything you want! Let's install i3 configuration for example.

```sh
stow i3
```

 > **WARNING!** Because of `.stowrc`, it will automatically overwrite any existing file with the same name. Be sure to backup your files if you don't want to lose your setup.
