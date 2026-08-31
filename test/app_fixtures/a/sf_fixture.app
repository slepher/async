{application, sf_fixture,
 [{description, "sf_fixture A"}, {vsn, "1.0.0"},
  {modules, [sf_fixture_app, sf_fixture_sup, order_flow, payment_flow]},
  {registered, [sf_fixture_sup, order_sup, order_worker, payment_sup, payment_worker]},
  {applications, [kernel, stdlib, sasl, async]},
  {mod, {sf_fixture_app, []}}, {env, [{marker, a}]}]}.
