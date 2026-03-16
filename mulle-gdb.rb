class MulleGdb < Formula
  desc "mulle-gdb, the mulle-objc debugger based on gdb"
  homepage "https://github.com/mulle-cc/mulle-gdb"
  license "GPL-3.0-or-later"
  version "11.1.0.1"

  url "https://github.com/mulle-cc/mulle-gdb/archive/refs/tags/11.1.0.1.tar.gz"
  sha256 "7499b14bc2b18f8499f77bdf28b2bc03df36aaa99bdc04d6d306d84674d7f05d"

  #
  # MEMO: For each macOS version, build bottles with:
  #
  #   brew uninstall mulle-objc/software/mulle-gdb
  #   brew install --formula --build-bottle mulle-gdb.rb
  #   brew tap-new mulle-objc/software   # if not already tapped
  #   cp mulle-gdb.rb $(brew --repository mulle-objc/software)/mulle-gdb.rb
  #   brew bottle mulle-objc/software/mulle-gdb
  #   # rename: mulle-gdb--11.1.0.1.sequoia.bottle.tar.gz
  #   #      -> mulle-gdb-11.1.0.1.sequoia.bottle.tar.gz
  #   gh release upload 11.1.0.1 mulle-gdb-11.1.0.1.*.bottle.tar.gz \
  #      --repo mulle-cc/mulle-gdb
  #
  # Then update the bottle do block below with the sha256 output.
  #
  # bottle do
  #   root_url "https://github.com/mulle-cc/mulle-gdb/releases/download/11.1.0.1/"
  #   sha256 cellar: :any_skip_relocation, sequoia:      "..."
  #   sha256 cellar: :any_skip_relocation, arm64_sequoia: "..."
  # end

  depends_on "ncurses"
  depends_on "gmp"
  # mpfr only available/needed on Linux for mulle-gdb
  on_linux do
    depends_on "mpfr"
  end

  on_system :linux, macos: :ventura_or_newer do
    depends_on "texinfo" => :build
  end

  def install
    args = %w[
      --program-prefix=mulle-
      --disable-binutils
      --disable-ld
      --disable-gold
      --disable-gas
      --disable-sim
      --disable-gprof
      --disable-gprofng
      --disable-libmcheck
      --enable-64-bit-bfd
      --with-curses
      --without-expat
      --without-guile
      --without-lzma
      --without-intel-pt
      --without-python
      --without-tcl
      --without-tk
      --with-tui
      --without-libunwind-ia64
      --without-x
      --without-babeltrace
      --enable-build-warnings=no
    ]

    # Fix `error: use of undeclared identifier 'startup_with_shell'` on macOS
    if OS.mac?
      inreplace "gdb/darwin-nat.c", "#include \"inferior.h\"",
        "#include \"inferior.h\"\n#include \"gdbsupport/common-inferior.h\""
    else
      args << "--with-gnu-ld"
    end

    # wipe config.cache files that persist CFLAGS across builds
    system "find", ".", "-name", "config.cache", "-delete"

    mkdir "build" do
      system "../configure", *args, *std_configure_args
      system "make", "-j#{ENV.make_jobs}", "MAKEINFO=true", "WERROR_CFLAGS="
      system "make", "install-gdb", "MAKEINFO=true"
    end
  end

  def caveats
    on_macos do
      <<~EOS
        mulle-gdb requires special privileges to access Mach ports.
        You will need to codesign the binary. For instructions, see:
          https://sourceware.org/gdb/wiki/PermissionsDarwin
      EOS
    end
  end

  test do
    system bin/"mulle-gdb", "--version"
  end
end
