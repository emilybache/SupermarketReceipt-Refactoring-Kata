module Approvals
  def verify(output)
    if File.exist?(approval_filename)
      assert_equal File.read(approval_filename), output
    elsif ENV['APPROVALS_RECORD'] == 'yes'
      File.open(approval_filename, 'w+') { |file| file.write(output) }
    else
      raise <<~MSG
        Some approvals are missing (#{approval_filename}).
        Try to run the test again with `APPROVALS_RECORD=yes` to record the expected results.
      MSG
    end
  end

  private

  def approval_filename
    File.join(
      File.dirname(__FILE__),
      "..",
      "approvals",
      "#{self.class.name}.#{self.name}.approved.txt",
    )
  end
end
