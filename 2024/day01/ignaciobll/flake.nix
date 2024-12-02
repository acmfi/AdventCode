{
  description = "Cuda flake with nvidia driver 550.135 and runtime 12.4";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    devShell.x86_64-linux =
      # let pkgs = nixpkgs.legacyPackages.x86_64-linux;
      let pkgs = import nixpkgs { system = "x86_64-linux"; config.allowUnfree = true; };
      in pkgs.mkShell {
        buildInputs = with pkgs; [
          cudatoolkit # Currently installs driver 12.4
          cudaPackages.cuda_cudart

          # Driver version from linuxPackages.nvidia_x11 is 560.x, which is
          # incompatible with 12.4. Until 12.6 is included in the cudatoolkit
          linuxPackages.nvidia_x11_production
        ];

        shellHook = ''
        export CUDA_PATH=${pkgs.cudatoolkit}
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.linuxPackages.nvidia_x11_production}/lib
        export EXTRA_LDFLAGS="-L/lib -L${pkgs.linuxPackages.nvidia_x11_production}/lib"
        export EXTRA_CCFLAGS="-I/usr/include"
        '';
      };
  };
}
