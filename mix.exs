defmodule erlte.MixProject do

  use Mix.Project

  def project() do
    [
      app: : erlte,
      version: "0.1.0",
      elixir: "~> 1.10",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      name: "erlte",
      source_url: "https://github.com/ergenius/erlte"
    ]
  end

  def application() do
    []
  end

  defp deps() do
    [
      {:ex_doc, "~> 0.27", only: :dev, runtime: false}
    ]
  end

  defp description() do
    "A few sentences (a paragraph) describing the project."
  end

  defp package() do
    [
      # This option is only needed when you don't want to use the OTP application name
      name: "erlte",
      # These are the default files included in the package
      files: ~w(lib priv .formatter.exs mix.exs README* LICENSE* CHANGELOG* src),
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/ergenius/erlte"}
    ]
  end

end