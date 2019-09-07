open TestFramework;

module Environment = Rench.Environment;
module EnvironmentVariables = Rench.EnvironmentVariables;
module Path = Rench.Path;

describe("Environment", ({describe, _}) => {
  describe("getEnvironmentVariables", ({test, _}) =>
    test("PATH is available", ({expect, _}) => {
      let env = Environment.getEnvironmentVariables();
      let expectedPath = Sys.getenv("PATH");

      let actualValue = EnvironmentVariables.getValue(env, "PATH");
      switch (actualValue) {
      | Some(v) => expect.string(v).toEqual(expectedPath)
      | _ => expect.bool(true).toBeFalse()
      };
    })
  )
});
