# Building a re-frame app

## Background

* Greenfield Erlang project to excercise some of the basics
* Haven't done frontend development in years
* Didn't know a thing about React (and still don't know much)
* Biased towards weird and cool techs

## The Project

* Small app to send country holiday reminders
  * Intended mostly for freelance developers
* https://holidayping.lambdaclass.com
* https://github.com/lambdaclass/holiday_ping

![HolidayPing channels](https://gist.githubusercontent.com/facundoolano/d385733c21e2cdffc3b6a15cef4aa3ea/raw/6af000e4b3e02ac28ae5538545e0cbd4d2114c02/channels.png)

![HolidayPing calendar](https://gist.githubusercontent.com/facundoolano/d385733c21e2cdffc3b6a15cef4aa3ea/raw/6af000e4b3e02ac28ae5538545e0cbd4d2114c02/calendar.png)

* PRs welcome!

## Re-frame

* `(-> react.js reagent re-frame)`
  * react: A declarative, efficient, and flexible JavaScript library
    for building user interfaces.
  * reagent: A minimalistic ClojureScript interface to React.js.
  * re-frame: A pattern for writing SPAs in ClojureScript, using Reagent.
* Similar architecture to Redux, Om Next and Elm.

## Views

* Just functions that produce HTML (hiccup)
* Should be ketp as dumb as possible: re-frame provides other
  components to handle reading/writing state.

``` clojure
(defn footer-view
  []
  [:footer.footer
   [:div.container
    [:div.content.has-text-centered
     [:p [:strong "HolidayPing"] " by "
      [:a {:href "https://github.com/lambdaclass/" :target "_blank"} "LambdaClass"] "."]
     [:p [:a.icon {:href "https://github.com/lambdaclass/holiday_ping"}
          [:i.fa.fa-github]]]]]])
```

## State: app-db

* Reagent provides `reagent/atom` to manage state: like
  `clojure.core/atom`, but it keeps track of every time it is deref’ed
* Any component that uses an atom is automagically re-rendered when its value changes.
* re-frame puts all your application state into single `reagent/atom`,
which is called app-db.

## Events

* Events allow to write to the db and handle other effectful
  interactions (i.e. talk to a server)
* Events are pure functions: they take effects as inputs and return
  new effects as data.

```clojure
(re-frame/reg-event-fx
 :auth-submit
 (fn [_ [_ {:keys [email password]}]]
   {:http-xhrio {:method          :get
                 :uri             "/api/auth/token"
                 :timeout         8000
                 :headers         {:authorization (basic-auth-header email password)}
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success      [:login-success]
                 :on-failure      [:login-failure]}}))

(re-frame/reg-event-fx
 :login-success
 (fn [{:keys [db]} [_ response]]
   (let [token (:access_token response)]
     {:dispatch        [:navigate :channel-list]
      :db              (assoc db :access-token token)
      :set-local-store ["access_token" token]})))
```

## Subscriptions

* Subscriptions allow to read/query the db.
* They help to make your views dumber by encapsulating all data
  processing.
* They allow to decouple the view from the app-db schema.

``` clojure
(re-frame/reg-sub
 :authenticated?
 (fn [db]
   (boolean (:access-token db))))

(re-frame/reg-sub
 :user-info
 (fn [db]
   (token/decode (:access-token db))))

(re-frame/reg-sub
 :avatar
 (fn [_ _] (re-frame/subscribe [:user-info]))
 (fn [{:keys [email avatar]} _]
   (if avatar
     avatar
     (gravatar email))))
```

## Views (again)

```clojure
(defn user-info-view []
  (when @(re-frame/subscribe [:authenticated?])
    (let [{name :name} @(re-frame/subscribe [:user-info])
          avatar       @(re-frame/subscribe [:avatar])]
      [:header.navbar-item.is-hoverable.has-dropdown
       [:a.navbar-link
        [:img {:src avatar}]]
       [:div.navbar-dropdown
        [:div.navbar-item.has-text-grey name]
        [:hr.navbar-divider]
        [:a.navbar-item
         {:on-click #(re-frame/dispatch [:logout])} "Logout"]]])))
```

![avatar component](https://gist.githubusercontent.com/facundoolano/d385733c21e2cdffc3b6a15cef4aa3ea/raw/f4bdfebaac6ead8f132c824512154a338ca33c64/avatar.png)


## Useful links

* [Introduction to Reagent](http://reagent-project.github.io/)
* [Guide to Reagent](https://purelyfunctional.tv/guide/reagent/)
* [Why Re-frame instead of Om Next](https://purelyfunctional.tv/article/why-re-frame-instead-of-om-next/)
* [Re-frame README](https://github.com/Day8/re-frame/blob/master/README.md)
* [Re-frame code walkthrough](https://github.com/Day8/re-frame/blob/master/docs/CodeWalkthrough.md)
* [The Re-frame Building Blocks Guide](https://purelyfunctional.tv/guide/re-frame-building-blocks/)
* [HolidayPing](https://github.com/lambdaclass/holiday_ping)
