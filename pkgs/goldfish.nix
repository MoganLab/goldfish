{
  lib,
  stdenv,

  buildPackages,
  windows,

  s7,
  tbox,
  isocline,

  static ? false,
}:

stdenv.mkDerivation {
  pname = "goldfish";
  version = "17.11.24";

  src = ./..;

  nativeBuildInputs = with buildPackages; [
    xmake
    # make xmake happy
    (writers.writeBashBin "git" "exit 0")
  ];
  buildInputs = [
    s7
    tbox
    isocline
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
      --repl=y --ccache=n             \
      --system-deps=y --pin-deps=n    \
    ${lib.optionalString stdenv.hostPlatform.isMinGW ''
      --toolchain=mingw --mingw=${stdenv.cc.outPath}
    ''}
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    xmake build --yes -j $NIX_BUILD_CORES -vD goldfish
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    # workaround for xmake cannot use system deps
    # when cross platform was set
    ${lib.optionalString stdenv.hostPlatform.isMinGW ''
      mv bin/goldfish.exe bin/goldfish
    ''}
    xmake install -vD -o $out goldfish
    ${lib.optionalString stdenv.hostPlatform.isMinGW ''
      mv $out/bin/goldfish $out/bin/goldfish.exe
    ''}

    runHook postInstall
  '';

  meta = {
    description = "R7RS-small scheme implementation based on s7 scheme";
    homepage = "https://gitee.com/XmacsLabs/goldfish";
    license = lib.licenses.asl20;
    mainProgram = "goldfish";
    platforms = lib.platforms.all;
    maintainers = with lib.maintainers; [ jinser ];
  };
}
