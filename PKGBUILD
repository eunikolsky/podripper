# Maintainer: Eugene Nikolsky <e@egeek.me>
pkgname=podripper
pkgver=2.4.2
pkgrel=1
pkgdesc="Rips online podcast streams and generates RSS for them"
arch=('x86_64')
url=""
license=('MIT')
depends=('glibc' 'gmp')
source=($pkgname.tar)
md5sums=('SKIP')

package() {
  cd "$pkgname"

  install -Dm 755 -t "$pkgdir"/usr/bin ripper-exe
  install -Dm 644 LICENSE "$pkgdir/usr/share/license/$pkgname/LICENSE"

  find conf -maxdepth 1 -type f -name '*.json' \
    -exec install -Dm 644 -t "$pkgdir"/usr/share/$pkgname "{}" +

  find systemd -maxdepth 1 -type f -name '*.service' \
    -exec install -Dm 644 -t "$pkgdir"/usr/lib/systemd/system "{}" +

  # note: uid=1000 is the default user on the system
  # TODO update user
  install -m 755 -o 1000 -d "$pkgdir"/var/lib/podripper
}
