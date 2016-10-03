# -*- mode: snippet; require-final-newline: nil -*-
# name: python package suitable for python-packages.nix
# key: pythonPackage
# expand-env: ((yas-indent-line 'fixed))
# binding: direct-keybinding/
# --
${1:name} = buildPythonPackage rec {
  name = "$1-\${version}";
  version = "$2";

  src = pkgs.fetchurl {
    url = "${3:https://pypi.python.org/packages/source/${1:$(substring yas-text 0 1)}/$1/$1-\${version}.tar.gz}";
    sha256 = "${0:`(my-insert-random-sha256)`}";
  };

  buildInputs = with self; [ $4 ];
  propagatedBuildInputs = with self; [ $5 ];

  meta = {
    description = "$6";
    homepage = "$7";
    maintainer = with maintainers; [ $8 ];
    license = licenses.$9;
  };
};
