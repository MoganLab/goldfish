{
  lib,
  stdenv,

  buildPackages,
  windows,

  tbox,

  static ? false,
}:

stdenv.mkDerivation {
  pname = "goldfish";
  version = "18.11.20";

  src = ./..;

  nativeBuildInputs = with buildPackages; [
    xmake
    # make xmake happy
    (writers.writeBashBin "git" "exit 0")
  ];
  buildInputs = [
    tbox
  ]
  ++ lib.optional stdenv.hostPlatform.isMinGW windows.pthreads;

  env.NIX_CFLAGS_COMPILE = toString (lib.optional static "-static");
  env.NIX_LDFLAGS = toString (
    lib.optionals stdenv.hostPlatform.isMinGW [
      "-lpthread"
      "-lws2_32"
    ]
  );

  configurePhase = ''
    runHook preConfigure
    export HOME=$(mktemp -d)
    xmake global --network=private
    xmake config -m release --yes -vD \
      --repl=n --ccache=n             \
      --system-deps=y --pin-deps=n    \
    ${lib.optionalString stdenv.hostPlatform.isMinGW ''
      --toolchain=mingw --mingw=${stdenv.cc.outPath}
    ''}
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    xmake build --yes -j $NIX_BUILD_CORES -vD gf-native
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    # workaround for xmake cannot use system deps
    # when cross platform was set
    ${lib.optionalString stdenv.hostPlatform.isMinGW ''
      mv bin/gf.exe bin/gf
    ''}
    xmake install -vD -o $out gf-native
    ${lib.optionalString stdenv.hostPlatform.isMinGW ''
      mv $out/bin/gf $out/bin/gf.exe
    ''}

    runHook postInstall
  '';

  meta = {
    description = "R7RS-small Scheme implementation with a native runtime";
    homepage = "https://gitee.com/XmacsLabs/goldfish";
    license = lib.licenses.asl20;
    mainProgram = "gf";
    platforms = lib.platforms.all;
    maintainers = with lib.maintainers; [ jinser ];
  };
}
