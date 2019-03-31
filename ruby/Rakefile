require "rake/testtask"

Rake::TestTask.new do |t|
  t.description = "Run tests"
  t.test_files = FileList["test/**/*_test.rb"]
end

task default: :test

namespace :test do
  desc "Run tests with coverage report"
  task :coverage do
    ENV["COVERAGE"] = "yes"
    Rake::Task["test"].execute
  end

  desc "Run tests with mutations (specify the class to mutate: rake test:mutation[Kata::Offer])"
  task :mutation, [:klass] do |_, args|
    sh "bundle exec mutant -I test --use minitest #{args.klass}" rescue 0
  end
end
