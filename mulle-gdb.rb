class MulleGdb < Formula
  desc "mulle-gdb, the mulle-objc debugger based on gdb"
  homepage "https://github.com/mulle-cc/mulle-gdb"
  license "GPL-3.0-or-later"
  version "16.3.0.1"

  url "https://github.com/mulle-cc/mulle-gdb/archive/refs/tags/16.3.0.1.tar.gz"
  sha256 "49c4b651da3ffddb809dd66c6ca086ee7e102f532312e8069f13ebcfab53fc3a"

  #
  # MEMO: For each macOS version, build bottles with:
  #
  #   brew uninstall mulle-objc/software/mulle-gdb
  #   brew install --formula --build-bottle mulle-gdb.rb
  #   brew tap-new mulle-objc/software   # if not already tapped
  #   cp mulle-gdb.rb $(brew --repository mulle-objc/software)/Formula/mulle-gdb.rb
  #   brew bottle mulle-objc/software/mulle-gdb
  #   # rename: mulle-gdb--16.3.0.1.arm64_sequoia.bottle.tar.gz
  #   #      -> mulle-gdb-16.3.0.1.arm64_sequoia.bottle.tar.gz
  #   gh release upload 16.3.0.1 mulle-gdb-16.3.0.1.*.bottle.tar.gz \
  #      --repo mulle-cc/mulle-gdb
  #
  # Then update the bottle do block below with the sha256 output.
  #
  bottle do
    root_url "https://github.com/mulle-cc/mulle-gdb/releases/download/16.3.0.1/"
    sha256 cellar: :any_skip_relocation, arm64_sequoia: "75ee4c878005e87d4d2f5857d2286ebf93c61b3bcd2c7b2cd9ae4760aa364f86"
  end

  depends_on "ncurses"
  depends_on "gmp"
  on_linux do
    depends_on "mpfr"
  end
  on_system :linux, macos: :ventura_or_newer do
    depends_on "texinfo" => :build
  end

  def install
    if OS.mac?
      inreplace "gdb/darwin-nat.c", "#include \"inferior.h\"",
        "#include \"inferior.h\"\n#include \"gdbsupport/common-inferior.h\""
    end

    # configure-mulle-gdb calls ./configure so must run from source root.
    # Pass --with-system-zlib to avoid bundled zlib/fdopen macro conflict on macOS.
    system "./configure-mulle-gdb", "--prefix=#{prefix}", "--with-system-zlib"
    system "make", "-j#{ENV.make_jobs}"
    system "make", "install"
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
