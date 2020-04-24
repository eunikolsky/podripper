# Maintainer: Eugene Nikolskyi <e@egeek.me>
pkgname=podripper
# TODO it's the version of `rssgen` for now
pkgver=0.1.4.0
pkgrel=1
epoch=
pkgdesc="Rips online podcast streams and generates RSS for them"
arch=('x86_64')
url=""
license=('unknown')
groups=()
depends=('bash' 'streamripper' 'ffmpeg' 'glibc' 'gmp')
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

  find conf -maxdepth 1 -type f -name '*.conf' \
    -exec install -Dm 644 -t "$pkgdir"/usr/share/$pkgname "{}" +

  find rssgen/conf -maxdepth 1 -type f -name '*.conf' \
    -exec install -Dm 644 -t "$pkgdir"/usr/share/$pkgname "{}" +

  find systemd -maxdepth 1 -type f \
    \( -name '*.service' -o -name '*.timer' \) \
    -exec install -Dm 644 -t "$pkgdir"/usr/lib/systemd/system "{}" +
}
