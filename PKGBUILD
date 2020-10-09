# Maintainer: Eugene Nikolsky <e@egeek.me>
pkgname=podripper
# TODO it's the version of `rssgen` for now
pkgver=0.2.2.2
pkgrel=1
epoch=
pkgdesc="Rips online podcast streams and generates RSS for them"
arch=('x86_64')
url=""
license=('MIT')
groups=()
depends=('bash' 'streamripper' 'ffmpeg' 'glibc' 'gmp' 'trash-cli')
makedepends=()
checkdepends=()
optdepends=()
provides=()
conflicts=()
replaces=()
backup=()
options=()
install=
changelog=
source=($pkgname-$pkgver.tar.gz)
noextract=()
md5sums=('SKIP')
validpgpkeys=()

package() {
  cd "$pkgname-$pkgver"

  install -Dm 755 -t "$pkgdir"/usr/bin podripper.sh rssgen-exe
  install -Dm 644 LICENSE "$pkgdir/usr/share/license/$pkgname/LICENSE"

  find conf -maxdepth 1 -type f -name '*.conf' \
    -exec install -Dm 644 -t "$pkgdir"/usr/share/$pkgname "{}" +

  find rssgen/conf -maxdepth 1 -type f -name '*.conf' \
    -exec install -Dm 644 -t "$pkgdir"/usr/share/$pkgname "{}" +

  find systemd -maxdepth 1 -type f \
    \( -name '*.service' -o -name '*.timer' \) \
    -exec install -Dm 644 -t "$pkgdir"/usr/lib/systemd/system "{}" +

  install -m 755 -o user -d "$pkgdir"/var/lib/podripper
}
